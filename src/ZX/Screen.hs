{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE FlexibleContexts #-}
module ZX.Screen where

import Control.DeepSeq
import Control.Lens ((^.))
import Control.Monad.ST (runST)
import Data.Array.Repa ((:.)(..), Z(Z))
import qualified Data.Array.Repa as R
import Data.Bits (testBit, xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as BV
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as SMV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Data.Word (Word8)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (pokeElemOff)
import Foreign.Ptr (Ptr, castPtr)
import Linear (R2, V2(..), V3(..), _x, _y, _yx, _z)
import Linear.Affine (Point(P))
import Linear.Vector ((^*), (^/), (^+^))

{-
newtype RowMajor r2 t = RowMajor { unRowMajor :: r2 t }
    deriving Eq

instance (R2 r2, Ord t) => Ord (RowMajor r2 t) where
    a `compare` b = a^._yx `compare` b^._yx
-}

logicalScreenSizeWH :: V2 Int
logicalScreenSizeWH = V2 logicalScreenW logicalScreenH

logicalScreenW :: Int
logicalScreenW = 256
{-# INLINE logicalScreenW #-}

logicalScreenH :: Int
logicalScreenH = 192
{-# INLINE logicalScreenH #-}

logicalScreenArea :: Int
logicalScreenArea = logicalScreenSizeWH^._x * logicalScreenSizeWH^._y

blockSize :: Int
blockSize = 8
{-# INLINE blockSize #-}

blockSizeSqr :: Int
blockSizeSqr = 64
{-# INLINE blockSizeSqr #-}

screenBlocksWH :: V2 Int
screenBlocksWH = fmap (`div` blockSize) logicalScreenSizeWH

xy :: (Num a) => a -> a -> Point V2 a
xy x y = P (V2 x y)

newtype Sprite8 = Sprite8 { sprite8Bytes :: SV.Vector Word8 }
sprite8 = Sprite8 . V.fromList

sprite8Points :: Sprite8 -> SV.Vector (Point V2 Int)
sprite8Points (Sprite8 bs) = V.fromList $ do
    (y, b) <- [0..] `zip` V.toList bs
    x <- [0..7]
    let -- We encode leftmost pixel on the MSB.
        value = testBit b (7 - fromIntegral x)
    if value then [xy x y] else []

type PixelPos = Point V2 Int
type ScreenBits = S.Set PixelPos

emptyBits :: ScreenBits
emptyBits = S.empty

drawSprite :: Sprite8 -> PixelPos -> ScreenBits -> ScreenBits
drawSprite sprite pos =
    let cpos = fmap fromIntegral pos
        points = V.map (cpos ^+^) (sprite8Points sprite)
        spriteBits = S.fromList . V.toList $ points
    in S.union spriteBits

-- * Color attribute block routines

data Intensity = NormalI | BrightI
instance NFData Intensity where rnf a = a `seq` ()

newtype Color = Color { colorIndex :: Int }

data ColorBlock = ColorBlock !Color !Color !Intensity
instance NFData ColorBlock where rnf a = a `seq` ()

type BlockIndex = Point V2 Int

data ScreenColors = ScreenColors
    { defaultColor :: !ColorBlock
    , colorOverrides :: !(M.Map BlockIndex ColorBlock)
    }

defaultColors :: ColorBlock -> ScreenColors
defaultColors cb = ScreenColors cb M.empty

setBlockColor :: ColorBlock -> BlockIndex -> ScreenColors -> ScreenColors
setBlockColor cb pos screenColors = screenColors
    { colorOverrides = M.insert pos cb (colorOverrides screenColors) }

-- * Color rendering routines

type RGB = V3 Word8

-- | End of range excluded.
pixelRangeForBlock :: Int -> (Int, Int)
pixelRangeForBlock b = (b*blockSize, (b+1)*blockSize)

pixelList b = let (s,e) = pixelRangeForBlock b in [s .. e-1]

writeToPtr = unsafeWriteToPtr

writeToPtrTest2 p _ = unsafeWriteToPtr p (B.pack [0, 255, 0])

writeToPtrTest p _ =
    let pw = castPtr p :: Ptr Word8
    in do
        pokeElemOff pw 0 255
        pokeElemOff pw 1 255
        pokeElemOff pw 2 10

unsafeWriteToPtr :: Ptr a -> B.ByteString -> IO ()
unsafeWriteToPtr p bs = B.unsafeUseAsCStringLen bs $ \(bp, len) ->
    copyBytes (castPtr p) bp len

screenToBytes :: ScreenBits -> ScreenColors -> Ptr Word8 -> IO ()
screenToBytes bits colors ptr = go 0 0
  where
    go :: Int -> Int -> IO ()
    go !x !y = do
        let !cx = x `div` blockSize
            !cy = y `div` blockSize
            !colorBlock = fetchColorBlock (xy cx cy)
            !fg = foreground colorBlock
            !bg = background colorBlock
            !offs = 3*(y*logicalScreenW + x)
            !col = {-# SCC "pixMember" #-}
                if S.member (xy x y) bits then fg else bg
            -- !col = cR
        pokeElemOff ptr (offs) (col^._z)
        pokeElemOff ptr (offs+1) (col^._y)
        pokeElemOff ptr (offs+2) (col^._x)
        let !xp = x + 1
            !newline = xp == logicalScreenW
            !y2 = if newline then y+1 else y
            !x2 = if newline then 0 else xp
        if y2 == logicalScreenH
            then return ()
            else go x2 y2
    --
    fetchColorBlock :: BlockIndex -> ColorBlock
    fetchColorBlock bi = case M.lookup bi (colorOverrides colors) of
        Just cb -> cb
        Nothing -> defaultColor colors
    foreground, background :: ColorBlock -> RGB
    foreground (ColorBlock c _ i) = colorToRGB c i
    background (ColorBlock _ c i) = colorToRGB c i

screenToBytes2 :: ScreenBits -> ScreenColors -> Ptr Word8 -> IO ()
screenToBytes2 bits colors ptr = go 0 0
  where
    go :: Int -> Int -> IO ()
    go !x !y = do
        let cx = x `div` blockSize
            cy = y `div` blockSize
            colorBlock = fetchColorBlock (xy cx cy)
            fg = foreground colorBlock
            bg = background colorBlock
            offs = 3*(y*logicalScreenW + x)
            col = {-# SCC "pixMember" #-}
                if S.member (xy x y) bits then fg else bg
        pokeElemOff ptr (offs) (col^._z)
        pokeElemOff ptr (offs+1) (col^._y)
        pokeElemOff ptr (offs+2) (col^._x)
        let xp = x + 1
            newline = xp == logicalScreenW
            y2 = if newline then y+1 else y
            x2 = if newline then 0 else xp
        if y2 == logicalScreenH
            then return ()
            else go x2 y2
    --
    fetchColorBlock :: BlockIndex -> ColorBlock
    fetchColorBlock bi = case M.lookup bi (colorOverrides colors) of
        Just cb -> cb
        Nothing -> defaultColor colors
    foreground, background :: ColorBlock -> RGB
    foreground (ColorBlock c _ i) = colorToRGB c i
    background (ColorBlock _ c i) = colorToRGB c i

data ColorFB = ColorFB !RGB !RGB

precalcColor :: ScreenColors -> BV.Vector ColorFB
precalcColor colors = V.fromList $ do
    cy <- [0..screenBlocksWH^._y]
    cx <- [0..screenBlocksWH^._x]
    let cb = fetchColorBlock (xy cx cy)
    return $! ColorFB (foreground cb) (background cb)
  where
    fetchColorBlock :: BlockIndex -> ColorBlock
    fetchColorBlock bi = case M.lookup bi (colorOverrides colors) of
        Just cb -> cb
        Nothing -> defaultColor colors
    --
    foreground, background :: ColorBlock -> RGB
    foreground (ColorBlock c _ i) = colorToRGB c i
    background (ColorBlock _ c i) = colorToRGB c i

precalcBits :: ScreenBits -> SV.Vector Bool
precalcBits bits = runST $ do
    v <- SMV.replicate logicalScreenArea False
    mapM_ (\p -> SMV.write v (p^._y*logicalScreenW + p^._x) True) (S.toList bits)
    SV.freeze v

screenToBytes3 :: ScreenBits -> ScreenColors -> Ptr Word8 -> IO ()
screenToBytes3 bits colors ptr = go 0
  where
    go :: Int -> IO ()
    go !idx = if idx == logicalScreenArea then return () else do
        let y = idx `div` logicalScreenW
            x = idx `mod` logicalScreenW
            cx = x `div` blockSize
            iy = idx `div` blockSizeSqr
            (ColorFB fg bg) = colVec V.! (iy + cx)
            offs = 3*idx
            col = {-# SCC "pixMember" #-}
                if bitVec V.! idx then fg else bg
        pokeElemOff ptr (offs) (col^._z)
        pokeElemOff ptr (offs+1) (col^._y)
        pokeElemOff ptr (offs+2) (col^._x)
        go $! idx+1
    --
    colVec = precalcColor colors
    bitVec = precalcBits bits

screenToBytes4 :: ScreenBits -> ScreenColors -> Ptr Word8 -> IO ()
screenToBytes4 bits colors ptr = go 0
  where
    go :: Int -> IO ()
    go !idx = if idx == logicalScreenArea then return () else do
        let y = idx `div` logicalScreenW
            x = idx `mod` logicalScreenW
            cx = x `div` blockSize
            iy = idx `div` blockSizeSqr
            (ColorFB fg bg) = colVec V.! (iy + cx)
            offs = 3*idx
            col = {-# SCC "pixMember" #-}
                if bitVec V.! idx then fg else bg
        pokeElemOff ptr (offs) (col^._z)
        pokeElemOff ptr (offs+1) (col^._y)
        pokeElemOff ptr (offs+2) (col^._x)
        go $! idx+1
    --
    colVec = precalcColor colors
    bitVec = precalcBits bits


{-
-- TODO switch to B array, gen ByteArray, directly poke?
arrayToVector :: (V.Vector v Word8) => R.Array R.D R.DIM2 RGB -> (V2 Int, v Word8)
arrayToVector a =
    let vec = V.fromList . concatMap mem
            . V.toList . R.toUnboxed . R.computeS
            $ a
        (Z :. h :. w) = R.extent a
    in ((V2 w h), vec)
  where
    mem (V3 r g b) = [b, g, r]  -- for little-endian.
-}

cR, cG, cB :: RGB
cR = V3 1 0 0
cG = V3 0 1 0
cB = V3 0 0 1

colorToRGB :: Color -> Intensity -> RGB
colorToRGB (Color i) bright =
    let cols = [black, blue, red, magenta, green, cyan, yellow, white]
        brightVals = SV.fromList $ map (^* brightMultiplier BrightI) cols
        normalVals = SV.fromList $ map (^* brightMultiplier NormalI) cols
        vals = case bright of
            NormalI -> normalVals
            BrightI -> brightVals
    in vals V.! (i `mod` V.length vals)
  where
    brightMultiplier BrightI = 255
    brightMultiplier NormalI = 205
    black = 0
    blue = cB
    red = cR
    magenta = cR + cB
    green = cG
    cyan = cG + cB
    yellow = cR + cG
    white = cR + cG + cB
