module ZX.Screen.Monad.Class where

import Data.Text (Text)
import ZX.Screen
    ( BlockIndex, Color, ColorBlock(ColorBlock), Intensity, PixelPos, Sprite8)

class (Monad m) => ZXScreen m where
    draw :: Sprite8 -> PixelPos -> m ()
    write :: Text -> PixelPos -> m ()
    fg :: Color -> BlockIndex -> m ()
    bg :: Color -> BlockIndex -> m ()
    intensity :: Intensity -> BlockIndex -> m ()
    color :: ColorBlock -> BlockIndex -> m ()
    color (ColorBlock f b i) pos = do
        fg f pos
        bg b pos
        intensity i pos
