{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_, evaluate, finally)
import Control.Monad (forM_, guard, when)
import Control.Monad.Trans.Reader
import Control.Lens
import Data.Bool (bool)
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import qualified Foreign.Ptr as F
import SDL
import Linear (V2(..), V4(..), _x, _y)
import Linear.Affine (Point(P))
import Linear.Vector ((^*), (^+^))

import ZX.Data.Chars (stringToSprites)
import ZX.Screen
import ZX.Screen.Monad.Class
import ZX.Screen.Monad.Pure (renderPureScreen)

import Game.Tiles

-- How blocky the pixels will seem.
scaleFactor = 3

main :: IO ()
main = do
    initializeAll
    getDisplays >>= print
    window <- createWindow "Trainy Day" defaultWindow
        { windowInitialSize = fmap ((scaleFactor*) . fromIntegral) logicalScreenSizeWH
        , windowPosition = Absolute (P (V2 5 5))
        , windowMode = Windowed
        }
    blitLoop window 0

-- TODO(robinp): Lots of duplication from zxhs-demo.

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
        -- TODO(robinp): this is a very lame frame timing method.
        threadDelay 30001
        blitThing renderer tex t
        go renderer tex (t+1)

withTexture :: Texture -> (F.Ptr Word8 -> IO ()) -> IO ()
withTexture t f =
    (lockTexture t Nothing >>= (f . F.castPtr . fst)) `finally` unlockTexture t

-- TODO really have to make stuff work transparently with either of
-- xy or block-xy, which should be different types.
xyOfBlock x y = xy (blockSize*x) (blockSize*y)

blitThing :: Renderer -> Texture -> Int -> IO ()
blitThing renderer tex t = do
    clear renderer
    withTexture tex . renderPureScreen $ do
        bg (Color 3) (xy 0 0)
        fg (Color 4) (xy 1 1)
        mapM_ (bg (Color 5)) [xy x 0 | x <- [0..31]]
        mapM_ (bg (Color 6)) [xy x 1 | x <- [0..31]]
        write "Code something to begin!" (xyOfBlock 0 0)
        drawCompound train (xyOfBlock 3 7)
        drawCompound carry (xyOfBlock 1 7)
        --
        draw (animate steamSequence 5 t) (xyOfBlock 4 6)
        fg (Color 5) (xy 4 6)
            
        mapM_ (draw trackH) [xy (x*8) 68 | x <- [0..31]]
    copy renderer tex Nothing Nothing
    present renderer

animate :: [Sprite8] -> Int -> Int -> Sprite8
animate sps tickDiv t =
    let idx = (t `div` tickDiv) `mod` length sps
    in sps !! idx

allFor a = flip runReaderT a . ReaderT

-- TODO should be common utility in ZxHs.
drawCompound sps pos =
    forM_ (sps `zip` [0..]) $ \(rowData, rowIndex) ->
        forM_ (rowData `zip` [0..]) $ \(sprite, colIndex) -> do
            -- TODO HACK make it possible to color all affected tiles
            fg (Color 1) (fmap (`div` blockSize) pos ^+^ xy colIndex rowIndex)
            draw sprite (pos ^+^ xy (colIndex * blockSize) (rowIndex * blockSize))
