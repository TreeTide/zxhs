{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_, evaluate, finally)
import Control.Monad (forM_, guard, when)
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

import ZX.Data.Chars (stringToSprites)
import ZX.Screen
import ZX.Screen.Monad.Class
import ZX.Screen.Monad.Pure (renderPureScreen)

main :: IO ()
main = do
    initializeAll
    getDisplays >>= print
    window <- createWindow "ZxHs" defaultWindow
        { windowInitialSize = fmap ((3*) . fromIntegral) logicalScreenSizeWH
        , windowPosition = Absolute (P (V2 5 5))
        , windowMode = Windowed
        }
    blitLoop window 0

train :: Sprite8
train = sprite8 [ 0x00, 0x70, 0x77, 0x52, 0x5E, 0x7F, 0x55, 0x22 ]
train1 = sprite8 [ 0x00, 0x70, 0x77, 0x52, 0x5E, 0x7F, 0x5F, 0x22 ]
train2 = sprite8 [ 0x00, 0x70, 0x77, 0x52, 0x5E, 0x7F, 0x55, 0x3E ]
train3 = sprite8 [ 0x00, 0x70, 0x77, 0x52, 0x5E, 0x7F, 0x7D, 0x22 ]

lorry1 :: Sprite8
lorry1 = sprite8 [ 0x00, 0x00, 0x80, 0x66, 0x18, 0xFF, 0xAA, 0x44 ]

dither1o2, dither3o8, dither1o4, dither1o8, dither1o16 :: Sprite8
dither1o2 = loop8 [0x55, 0xAA]
dither3o8 = loop8 [0x55, 0x22, 0x55, 0x88]
dither1o4 = loop8 [0x55, 0x00]
dither1o8 = loop8 [0x44, 0x00, 0x11, 0x00]
dither1o16 = loop8 [0x00, 0x00, 0x11, 0x00]

loop8 = sprite8 . take 8 . cycle

dithers =
    [ dither1o2
    , dither3o8
    , dither1o4
    , dither1o4
    , dither1o8
    , dither1o16
    , dither1o16
    , dither1o16
    ]

blitLoop :: Window -> Int -> IO ()
blitLoop window t = do
    renderer <- createRenderer window (-1) defaultRenderer
        { rendererTargetTexture = True
        , rendererType = AcceleratedRenderer
        }
    texture <- createTexture renderer
        RGB24  -- This doesn't have any byte gap, while RGB888 seems to.
        TextureAccessStreaming
        (fmap fromIntegral logicalScreenSizeWH)
    go renderer texture t
  where
    go renderer tex !t = do
        events <- pollEvents
        threadDelay 30001
        blitThing renderer tex t
        -- if t < 100 then
        go renderer tex (t+1)
        -- else return ()

withTexture :: Texture -> (F.Ptr Word8 -> IO ()) -> IO ()
withTexture t f =
    (lockTexture t Nothing >>= (f . F.castPtr . fst)) `finally` unlockTexture t

xyOfBlock x y = xy (blockSize*x) (blockSize*y)

blitThing :: Renderer -> Texture -> Int -> IO ()
blitThing renderer tex t = do
    clear renderer
    withTexture tex . renderPureScreen $ do
        bg (Color 3) (xy 0 0)
        fg (Color 4) (xy 1 1)
        mapM_ (bg (Color 5)) [xy x 0 | x <- [0..31]]
        mapM_ (bg (Color 6)) [xy x 1 | x <- [0..31]]
        forM_ [0..7] $ \y -> do
            write "Hello ZxHs!" (xyOfBlock 0 y)
            let trainY = 12*8 + 4  -- Offset half block so it's funnier.
            draw train (xy trainY (y*8))
            draw lorry1 (xy (trainY-8) (y*8))
            forM_ [0..31] $ \x -> do
                let pos = xy x y
                    bgCol = colorNum x
                    fgCol = colorNum y
                    rightSide = x >= 16
                    fgColDifferent = if fgCol == bgCol && not rightSide
                                     then nextColor fgCol else fgCol
                    bright = if odd (x `div` 8)
                             then brightI else normalI
                color (ColorBlock fgColDifferent bgCol bright) (xy x y)
                when rightSide $ do
                    color (ColorBlock fgCol bgCol bright) (xy x (y+8))
                    color (ColorBlock fgCol bgCol bright) (xy x (y+16))
                    draw dither1o2 (xyOfBlock x y)
                    draw dither1o4 (xyOfBlock x (y+8))
                    draw dither1o8 (xyOfBlock x (y+16))
        forM_ [0..15] $ \x ->
            forM_ (reverse dithers `zip` [0..7]) $ \(sprite, y) -> do
                let top = xy x (8+y)
                    bottom = xy x (23-y)
                    col = ColorBlock (colorNum 1) (colorNum 7) normalI
                color col top
                draw sprite (fmap (*8) top)
                color (swapColors col) bottom
                draw sprite (fmap (*8) bottom)
    copy renderer tex Nothing Nothing
    present renderer

ani t = case (t `mod` 12) `div` 3 of
    0 -> train
    1 -> train1
    2 -> train2
    3 -> train3
    _ -> train
