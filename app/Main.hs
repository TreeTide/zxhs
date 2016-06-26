{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_, evaluate, finally)
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
        { windowInitialSize = fmap ((3*) . fromIntegral) logicalScreenSizeWH
        , windowPosition = Absolute (P (V2 5 5))
        }
    blitLoop window 0

train :: Sprite8
train = sprite8 [ 0x00, 0x70, 0x77, 0x52, 0x5E, 0x7F, 0x55, 0x22 ]

blitLoop :: Window -> Int -> IO ()
blitLoop window t = do
    renderer <- createRenderer window (-1) defaultRenderer
        { rendererTargetTexture = True }
    texture <- createTexture renderer
        RGB24  -- This doesn't have any byte gap, while RGB888 seems to.
        TextureAccessStreaming
        (fmap fromIntegral logicalScreenSizeWH)
    go renderer texture t
  where
    go renderer tex !t = do
        print ("Loop", t)
        events <- pollEvents
        threadDelay 30001
        blitThing renderer tex t
        if t < 100 then go renderer tex (t+1) else return ()

withTexture :: Texture -> (F.Ptr () -> IO ()) -> IO ()
withTexture t f =
    (lockTexture t Nothing >>= (f . fst)) `finally` unlockTexture t

makeBuffer :: IO Surface
makeBuffer = createRGBSurface (fmap fromIntegral logicalScreenSizeWH) 24 mask
  where
    mask = V4 0xFF0000 0x00FF00 0x0000FF 0x000000

blitThing :: Renderer -> Texture -> Int -> IO ()
blitThing renderer tex t = do
    let screen = foldr (drawSprite train) emptyBits
            [xy (x*10+y+(mod t 30)) (10*y) | x <- [1..20], y <- [3..15]]
        colors = setBlockColor (ColorBlock (Color 7) (Color 0) NormalI) (xy 0 0)
               . setBlockColor (ColorBlock (Color 4) (Color 2) NormalI) (xy 5 5)
               $ defaultColors (ColorBlock (Color 3) (Color 7) BrightI)
    withTexture tex (screenToBytes4 (bitsToWords screen) colors . F.castPtr)
    copy renderer tex Nothing Nothing
    present renderer
