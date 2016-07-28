{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
module ZX.Screen.Monad.Pure
    ( PureScreen
    ) where

import Control.Lens
import Control.Monad (foldM_)
import Control.Monad.Trans.State
import Data.Text (Text)
import Linear (_x)
import ZX.Data.Chars (textToSprites)
import ZX.Screen
    ( BlockIndex, Color, ColorBlock(ColorBlock)
    , DisplayList, DisplaySprite(..)
    , Intensity, PixelPos
    , ScreenColors, Sprite8
    , blockSize
    , colorOverrides
    , displaySprites
    , fgCB, bgCB, intensityCB
    )
import ZX.Screen.Monad.Class

data ScreenState = ScreenState
    { _display :: !DisplayList
    , _colors :: !ScreenColors
    }

makeLenses ''ScreenState

type PureScreen m = StateT ScreenState m

instance (Monad m) => ZXScreen (PureScreen m) where
    -- draw :: Sprite8 -> PixelPos -> PureScreen ()
    draw sprite pos = display.displaySprites %= cons (DisplaySprite pos sprite)

    -- write :: Text -> PixelPos -> PureScreen ()
    write text pos = foldM_ drawAndOffset pos (textToSprites text)
      where
        drawAndOffset pos charSprite = do
            draw charSprite pos
            return $! pos & _x +~ blockSize

    -- fg :: Color -> BlockIndex -> PureScreen ()
    fg c bi = colors.colorOverrides.at bi %= over _Just (fgCB .~ c)

    -- bg :: Color -> BlockIndex -> PureScreen ()
    bg c bi = colors.colorOverrides.at bi %= over _Just (bgCB .~ c)

    -- intensity :: Intensity -> BlockIndex -> PureScreen ()
    intensity i bi = colors.colorOverrides.at bi %=
        over _Just (intensityCB .~ i)

    -- color :: ColorBlock -> BlockIndex -> PureScreen ()
    color cb bi = colors.colorOverrides.at bi ?= cb
