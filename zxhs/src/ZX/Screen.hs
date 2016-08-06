{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module ZX.Screen where

import Control.DeepSeq
import Control.Lens ((^.), (?~), at, makeLenses)
import Control.Monad (forM_, unless)
import Control.Monad.ST (runST, ST)
import Data.Bits ((.|.), setBit, shiftL, shiftR, testBit, xor)
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
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, castPtr)
import Linear (R2, V2(..), V3(..), _x, _y, _yx, _z)
import Linear.Affine (Point(P))
import Linear.Vector ((^*), (^/), (^+^))

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

data DisplaySprite = DisplaySprite
    { _posDS :: PixelPos
    , _spriteDS :: Sprite8
    }
makeLenses ''DisplaySprite

data DisplayList = DisplayList
    { _displaySprites :: [DisplaySprite] }

makeLenses ''DisplayList

emptyBits :: DisplayList
emptyBits = DisplayList []

drawSprite :: Sprite8 -> PixelPos -> DisplayList -> DisplayList
drawSprite sprite pos (DisplayList sprites) =
    DisplayList (DisplaySprite pos sprite:sprites)

-- * Color attribute block routines

data Intensity = NormalI | BrightI
instance NFData Intensity where rnf a = a `seq` ()
-- | Smart constructors for creating intensity values.
-- Prefer these to using the ctors directly.
normalI, brightI :: Intensity
normalI = NormalI
brightI = BrightI
-- | Case selection on Intensity, abstracts away the actual representation.
foldI :: a -> a -> Intensity -> a
foldI normal bright intensity = case intensity of
    NormalI -> normal
    BrightI -> bright

-- | Number of base colors supported by the ZX Spectrum.
-- Does not include variations due to intensity.
colorCount :: Int
colorCount = 8

-- | Index into the standard Spectrum colors, ranging from 0 to 'colorCount'-1.
newtype Color = Color { colorIndex :: Word8 }
    deriving (Eq, Ord)

-- | Smart constructor for 'Color', mapping the argument into the allowed color
-- range using modulus (the color numbers wrap around).
colorNum :: Int -> Color
colorNum = Color . fromIntegral . (`mod` colorCount)

nextColor :: Color -> Color
nextColor (Color i) = colorNum (fromIntegral $ i + 1)

data ColorBlock = ColorBlock
    { _fgCB :: !Color
    , _bgCB :: !Color
    , _intensityCB :: !Intensity
    }
makeLenses ''ColorBlock

instance NFData ColorBlock where rnf a = a `seq` ()

swapColors :: ColorBlock -> ColorBlock
swapColors (ColorBlock f b i) = ColorBlock b f i

-- TODO(robinp): stronger type to avoid mixing up block and pixel coords.
type BlockIndex = Point V2 Int

data ScreenColors = ScreenColors
    { _blocksSC :: !(M.Map BlockIndex ColorBlock) }
makeLenses ''ScreenColors

defaultColors :: ScreenColors
defaultColors = ScreenColors M.empty

setBlockColor :: ColorBlock -> BlockIndex -> ScreenColors -> ScreenColors
setBlockColor cb pos = blocksSC.at pos ?~ cb

-- * Color rendering routines

type RGB = V3 Word8

-- | Isomorphic to 'V3 Word8'.
-- Used in the 'ColorTable', so the blitting code allocates less.
data UnpackRGB = UnpackRGB !Word8 !Word8 !Word8

instance Storable UnpackRGB where
    sizeOf _ = 3 * sizeOf (undefined :: Word8)
    {-# INLINE sizeOf #-}

    alignment _ = alignment (undefined :: Word8)
    {-# INLINE alignment #-}

    poke ptr (UnpackRGB r g b) = do
        poke ptr' r
        pokeElemOff ptr' 1 g
        pokeElemOff ptr' 2 b
      where ptr' = castPtr ptr
    {-# INLINE poke #-}

    peek ptr = UnpackRGB
        <$> peek ptr'
        <*> peekElemOff ptr' 1
        <*> peekElemOff ptr' 2
      where ptr' = castPtr ptr
    {-# INLINE peek #-}

-- | A foreground and background color used in a given color block.
data ColorFB = ColorFB {-# UNPACK #-} !UnpackRGB {-# UNPACK #-} !UnpackRGB

instance Storable ColorFB where
    sizeOf _ = 2 * sizeOf (undefined :: UnpackRGB)
    {-# INLINE sizeOf #-}

    alignment _ = alignment (undefined :: UnpackRGB)
    {-# INLINE alignment #-}

    poke ptr (ColorFB f b) = do
        poke ptr' f
        pokeElemOff ptr' 1 b
      where ptr' = castPtr ptr
    {-# INLINE poke #-}

    peek ptr = ColorFB <$> peek ptr' <*> peekElemOff ptr' 1
      where ptr' = castPtr ptr
    {-# INLINE peek #-}

-- | Repacks a pair of 'RGB' colors as unpacked 'ColorFB'.
mkColorFB :: RGB -> RGB -> ColorFB
mkColorFB fg bg = ColorFB (unpackRGB fg) (unpackRGB bg)
  where
    unpackRGB (V3 r g b) = UnpackRGB r g b

-- | Color blocks of the screen, indexed by the row-major index of the
-- 'blockSize'*'blockSize' (here 8x8) pixel large blocks.
type ColorTable = SV.Vector ColorFB

calcColorTable :: ScreenColors -> ColorTable
calcColorTable colors = V.fromList $ do
    cy <- [0..screenBlocksWH^._y - 1]
    cx <- [0..screenBlocksWH^._x - 1]
    let cb = fetchColorBlock (xy cx cy)
    return $! mkColorFB (foreground cb) (background cb)
  where
    fetchColorBlock :: BlockIndex -> ColorBlock
    fetchColorBlock bi = M.findWithDefault defaultColor bi (_blocksSC colors)
    --
    foreground, background :: ColorBlock -> RGB
    foreground (ColorBlock c _ i) = colorToRGB c i
    background (ColorBlock _ c i) = colorToRGB c i

defaultColor :: ColorBlock
defaultColor = ColorBlock (Color 0) (Color 7) NormalI

type ScreenWords = SV.Vector Word8

-- TODO(robinp): do bounds checking to clip pixels.
-- Idea: specifiable bit operation (now fixed OR)?
bitsToWords :: DisplayList -> ScreenWords
bitsToWords bits = SV.create $ do
    v <- SMV.replicate (logicalScreenArea `div` blockSize) 0x00
    forM_ (_displaySprites bits) $ \(DisplaySprite p sprite) -> do
        let bitIndex = p^._y*logicalScreenW + p^._x
            byteIndex = bitIndex `div` blockSize
            bitOffs = bitIndex `mod` blockSize
        if bitOffs == 0
        then drawAligned v (sprite8Bytes sprite) byteIndex
        else drawWithBitOffset v (sprite8Bytes sprite) byteIndex bitOffs
    return $! v
  where
    drawAligned :: SMV.STVector s Word8 -> SV.Vector Word8 -> Int -> ST s ()
    drawAligned v s byteIndex = go 0 byteIndex
      where
        go !x _ | x == V.length s = return $! ()
        go !x !bi = do
            prev <- SMV.read v bi
            let spriteLine = s V.! x
            SMV.write v bi (prev .|. spriteLine)
            go (x+1) (bi + screenBlocksWH^._x)
    --
    drawWithBitOffset
        :: SMV.STVector s Word8 -> SV.Vector Word8 -> Int -> Int -> ST s ()
    drawWithBitOffset v s byteIndex bitOffset = go 0 byteIndex
      where
        go !x _ | x == V.length s = return $! ()
        go !x !bi = do
            -- Still missing boundary checks.
            prevLeft <- SMV.read v bi
            prevRight <- SMV.read v (bi+1)
            let spriteLine = s V.! x
                spriteLeft = shiftR spriteLine bitOffset
                spriteRight = shiftL spriteLine (8 - bitOffset)
            SMV.write v bi (prevLeft .|. spriteLeft)
            SMV.write v (bi+1) (prevRight .|. spriteRight)
            go (x+1) (bi + screenBlocksWH^._x)

-- | Core blitting routine, combines the separate pixel and color information,
-- and writes the colored pixels to the pointed area. There must be sufficient
-- space in that area ('logicalScreenArea' * 3 bytes)!
screenToBytes4 :: ScreenWords -> ColorTable -> Ptr Word8 -> IO ()
screenToBytes4 words colors ptr = go 0
  where
    go :: Int -> IO ()
    go !block | block == (logicalScreenArea `div` blockSize) = return $! ()
    go !block = do
        let cx = block `mod` screenBlocksWH^._x
            cy = block `div` screenBlocksWH^._x `div` blockSize
            coffs = cy*(screenBlocksWH^._x) + cx
            offs = 3*block*blockSize
        -- Note: either indexM or unsafeIndex allocate a lot less than plain !,
        -- so using the monadic one (though bounds should be ok here).
        colorAttrib <- V.indexM colors coffs
        w <- V.indexM words block
        goWord ptr w colorAttrib offs
        go (block+1)

-- | Used locally by blitting routine, but generated code is better by making
-- this separation (see readme in profiles).
goWord :: Ptr Word8 -> Word8 -> ColorFB -> Int -> IO ()
goWord ptr !w !(ColorFB fg bg) !offs = go 7 offs
  where
    go :: Int -> Int -> IO ()
    go !x !offs = do
        let UnpackRGB r g b = if testBit w x then fg else bg
        pokeElemOff ptr (offs) r
        pokeElemOff ptr (offs+1) g
        pokeElemOff ptr (offs+2) b
        unless (x == 0) $! go (x-1) (offs+3)

colorToRGB :: Color -> Intensity -> RGB
colorToRGB (Color i) bright =
    let cols = [black, blue, red, magenta, green, cyan, yellow, white]
        brightVals = SV.fromList $ map (^* brightMultiplier brightI) cols
        normalVals = SV.fromList $ map (^* brightMultiplier normalI) cols
        -- | Not using a single, bright-dependent mapping, to faciliate sharing.
        vals = foldI normalVals brightVals bright
       -- TODO(robinp): use smart ctor for Color and do the modulus there.
    in vals V.! (fromIntegral i)
  where
    brightMultiplier = foldI 205 255
    black = 0
    blue = cB
    red = cR
    magenta = cR + cB
    green = cG
    cyan = cG + cB
    yellow = cR + cG
    white = cR + cG + cB
    --
    cR, cG, cB :: RGB
    cR = V3 1 0 0
    cG = V3 0 1 0
    cB = V3 0 0 1
