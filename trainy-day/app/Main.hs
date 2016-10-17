{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_, evaluate, finally)
import Control.Monad (forM_, guard, when)
import Control.Monad.Trans.Reader
import Control.Lens
import Data.Bool (bool)
import qualified Data.Map.Strict as M
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

import Game.Logic
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

game0 :: GS
game0 = GS r0 [t0, t1] 0
  where
    r0 :: Rails
    r0 = M.fromList  -- A circle
        [ track 5 5 D R
        , track 6 5 L R
        , track 7 5 L D
        , track 7 6 U D
        , track 7 7 U L
        , track 6 7 R L
        , track 5 7 R U
        , track 5 6 D U 
        ]
    --
    track i j f t = (pt i j, Left (Track f t))
    pt i j = P (V2 i j)
    --
    t0 = Train 1 (initOn r0 6 5)
    t1 = Train 2 (initOn r0 6 7) & engine.movingForward .~ False
    --
    initOn :: Rails -> Int -> Int -> Moving
    initOn r i j = case M.lookup (pt i j) r of
        Just (Left t) -> Moving (pt i j) t True 50
        Just (Right _) -> error "Switches unsupported yet"
        Nothing -> error "Bad train pos while initing"

renderRails :: ZXScreen m => Rails -> m ()
renderRails = mapM_ (uncurry renderTrack) . M.toList
  where
    renderTrack (P (V2 i j)) st = case st of
        Left t  -> draw (trackGfx t) (xyOfBlock i j)
        Right _ -> write "?" (xyOfBlock i j)
    --
    trackGfx = foldTrackDir trackH bendUL bendDL bendUR bendDR trackV

renderTrain :: ZXScreen m => Int -> Train -> m ()
renderTrain steps t =
    let unitOffset = (round . (*(fromIntegral blockSize))) <$> positionInTile (t^.engine)
        tileCorner = (blockSize*) <$> (t^.engine.tilePos)
        trainPivot = xy (-8) (-16)  -- TODO manage sprite pivots less manualy
        pos = tileCorner ^+^ unitOffset ^+^ trainPivot
        cloudOffset = xy 8 (-8)
    in do
        -- TODO mirror based on heading direction
        drawCompound train pos
        -- TODO kind of hack now, have rather separate cloud decals as entities
        draw (animate steamSequence 5 (steps + (7*t^.number))) (pos ^+^ cloudOffset)

renderGS :: ZXScreen m => GS -> m ()
renderGS gs = do
    renderRails (gs^.rails)
    mapM_ (renderTrain (gs^.gameStep)) (gs^.trains)

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
        --
        renderGS (update t game0)
        {-
        drawCompound train (xyOfBlock 3 7)
        drawCompound carry (xyOfBlock 1 7)
        --
        draw (animate steamSequence 5 t) (xyOfBlock 4 6)
        fg (Color 5) (xy 4 6)
            
        mapM_ (draw trackH) [xy (x*8) 68 | x <- [0..31]]
        -}
    copy renderer tex Nothing Nothing
    present renderer
  where
    -- For now a very dumb update, doesn't deal with track transitions etc.
    update t g = g & trains.each.engine.progress +~ t & gameStep .~ t

animate :: [Sprite8] -> Int -> Int -> Sprite8
animate sps tickDiv t =
    let idx = (t `div` tickDiv) `mod` length sps
    in sps !! idx

allFor a = flip runReaderT a . ReaderT

-- TODO should be common utility in ZxHs.
drawCompound multi pos =
    traverseMulti multi $ \(Element elemPos sprite) -> do
        let rowIndex = elemPos^._y
            colIndex = elemPos^._x
            -- TODO HACK make it possible to color all affected tiles (considering overlap)
            pixelPos = pos ^+^ xy (colIndex * blockSize) (rowIndex * blockSize)
            blockPos = fmap (`div` blockSize) pos ^+^ xy colIndex rowIndex
        fg (Color 1) blockPos
        draw sprite pixelPos
