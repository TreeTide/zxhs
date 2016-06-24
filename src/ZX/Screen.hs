{-# LANGUAGE FlexibleContexts #-}
module ZX.Screen where

import Data.Array.Repa ((:.)(..), Z(Z))
import qualified Data.Array.Repa as R
import Data.Bits (testBit, xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Map.Strict as M
import Control.Lens ((^.))
import qualified Data.Set as S
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as SV
import Data.Word (Word8)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (pokeElemOff)
import Foreign.Ptr (Ptr, castPtr)
import Linear (R2, V2(..), V3(..), _x, _y, _yx)
import Linear.Affine (Point(P))
import Linear.Vector ((^*), (^/), (^+^))

{-
newtype RowMajor r2 t = RowMajor { unRowMajor :: r2 t }
    deriving Eq

instance (R2 r2, Ord t) => Ord (RowMajor r2 t) where
    a `compare` b = a^._yx `compare` b^._yx
-}

logicalScreenSizeWH :: V2 Int
logicalScreenSizeWH = V2 256 192

blockSize :: Int
blockSize = 8

screenBlocksWH :: V2 Int
screenBlocksWH = fmap (`div` blockSize) logicalScreenSizeWH

xy :: (Num a) => a -> a -> Point V2 a
xy x y = P (V2 x y)
{-# INLINE xy #-}

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
newtype Color = Color { colorIndex :: Int }

data ColorBlock = ColorBlock !Color !Color !Intensity

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

pixelRangeForBlock :: Int -> [Int]
pixelRangeForBlock b = [b*blockSize .. (b+1)*blockSize - 1]

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

screenToBytes :: ScreenBits -> ScreenColors -> B.ByteString
screenToBytes bits colors = B.pack $ do
    cy <- [0..screenBlocksWH^._y - 1]
    y <- pixelRangeForBlock cy
    cx <- [0..screenBlocksWH^._x - 1]
    let colorBlock = fetchColorBlock (xy cx cy)
        fg = rgbToList (foreground colorBlock)
        bg = rgbToList (background colorBlock)
    x <- pixelRangeForBlock cx
    if S.member (xy x y) bits
    then fg
    else bg
  where
    fetchColorBlock :: BlockIndex -> ColorBlock
    fetchColorBlock bi = case M.lookup bi (colorOverrides colors) of
        Just cb -> cb
        Nothing -> defaultColor colors
    foreground, background :: ColorBlock -> RGB
    foreground (ColorBlock c _ i) = colorToRGB c i
    background (ColorBlock _ c i) = colorToRGB c i
    rgbToList :: RGB -> [Word8]
    rgbToList (V3 r g b) = [b, g, r]  -- little endian memory layout

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
        brightVals = map (^* brightMultiplier BrightI) cols
        normalVals = map (^* brightMultiplier NormalI) cols
        vals = case bright of
            NormalI -> normalVals
            BrightI -> brightVals
    in vals !! (i `mod` length vals)
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
