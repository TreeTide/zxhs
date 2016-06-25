{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_, evaluate)
import Control.Monad (forM_, guard)
import Control.Lens
import qualified Data.Set as S
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as SV
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import qualified Foreign.Ptr as F
import qualified Foreign.Storable as F
import SDL
import Linear (V2(..), V4(..), _x, _y)
import Linear.Affine (Point(P))
import Linear.Vector ((^*), (^+^))

import ZX.Screen

main :: IO ()
main = do
    initializeAll
    getDisplays >>= print
    window <- createWindow "ZxHs" defaultWindow
        { windowInitialSize = fmap fromIntegral logicalScreenSizeWH
        , windowPosition = Absolute (P (V2 5 5))
        }
    blitLoop window 0

train :: Sprite8
train = sprite8 [ 0x00, 0x70, 0x77, 0x52, 0x5E, 0x7F, 0x55, 0x22 ]

blitLoop :: Window -> Int -> IO ()
blitLoop window t = do
    buffer <- makeBuffer
    go buffer t
  where
    go buf !t = do
        print ("Loop", t)
        events <- pollEvents
        threadDelay 30001
        blitThing window buf t
        if t < 100 then go buf (t+1) else return ()

makeBuffer :: IO Surface
makeBuffer = createRGBSurface (fmap fromIntegral logicalScreenSizeWH) 24 mask
  where
    mask = V4 0xFF0000 0x00FF00 0x0000FF 0x000000

blitThing :: Window -> Surface -> Int -> IO ()
blitThing w buffer t = do
    let screen = foldr (drawSprite train) emptyBits [xy (10+t) 10, xy 40 10]
        colors = defaultColors (ColorBlock (Color 3) (Color 7) BrightI)
    winSurf <- getWindowSurface w
    bracket_ (lockSurface buffer) (unlockSurface buffer) $ do
        bufPtr <- surfacePixels buffer
        -- screenToBytes3 screen colors (F.castPtr bufPtr)
        screenToBytes4 (bitsToWords screen) colors (F.castPtr bufPtr)
        return ()
    surfaceBlit buffer Nothing winSurf Nothing
    updateWindowSurface w
