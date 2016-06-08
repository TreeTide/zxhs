{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (bracket)
import Control.Monad (forM_, guard)
import Data.Bits (testBit, xor)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as SV
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import qualified Foreign.Ptr as F
import qualified Foreign.Storable as F
import SDL
import Linear (V2(..), V4(..))
import Linear.Affine (Point(P))
import Linear.Vector ((^*))

main :: IO ()
main = do
    initializeAll
    getDisplays >>= print
    window <- createWindow "My SDL Application" defaultWindow
        { windowInitialSize = V2 1024 768
        , windowPosition = Absolute (P $ V2 5 5)
        }
    renderer <- createRenderer window (-1) defaultRenderer
    rendererLogicalSize renderer $= Just (V2 256 192)
    appLoop renderer

cR, cG, cB :: V4 Word8
cR = V4 1 0 0 0
cG = V4 0 1 0 0
cB = V4 0 0 1 0

data Intensity = NormalI | BrightI

spectrum :: Int -> Intensity -> V4 Word8
spectrum i bright =
    let cols = [black, blue, red, magenta, green, cyan, yellow, white]
        vals = map (^* brightMultiplier bright) cols
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

newtype Sprite8 = Sprite8 { sprite8Bytes :: SV.Vector Word8 }

sprite8 = Sprite8 . V.fromList

data Scape = InkS | PenS

sprite8Points :: Scape -> Sprite8 -> SV.Vector (Point V2 CInt)
sprite8Points scape (Sprite8 bs) = V.fromList $ do
    (y, b) <- [0..] `zip` V.toList bs
    x <- [0..7]
    let masked = b `xor` scapeMask scape
        -- We encode leftmost pixel on the MSB.
        value = testBit masked (7 - fromIntegral x)
    if value then [P (V2 x y)] else []
  where
    scapeMask :: Scape -> Word8
    scapeMask InkS = 0xFF
    scapeMask PenS = 0x00

train = sprite8 [ 0x00, 0x70, 0x77, 0x52, 0x5E, 0x7F, 0x55, 0x22 ]

appLoop :: Renderer -> IO ()
appLoop r = do
    events <- pollEvents
    rendererDrawColor r $= spectrum 1 NormalI
    clear r
    rendererDrawColor r $= spectrum 3 BrightI
    drawPoints r . sprite8Points PenS $ train
    present r
    appLoop r
