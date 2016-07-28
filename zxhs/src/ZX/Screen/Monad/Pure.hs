{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
module ZX.Screen.Monad.Pure
    ( PureScreen
    , renderPureScreen
    ) where

import Control.Lens
import Control.Monad (foldM_)
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Text (Text)
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Linear (_x)
import ZX.Data.Chars (textToSprites)
import ZX.Screen
    ( BlockIndex, Color, ColorBlock(ColorBlock)
    , DisplayList(..), DisplaySprite(..)
    , Intensity, PixelPos
    , ScreenColors, blocksSC, defaultColors, defaultColor
    , Sprite8
    , blockSize
    , displaySprites
    , fgCB, bgCB, intensityCB
    , bitsToWords, calcColorTable, screenToBytes4
    )
import ZX.Screen.Monad.Class

data ScreenState = ScreenState
    { _display :: !DisplayList
    , _colors :: !ScreenColors
    }

makeLenses ''ScreenState

type PureScreen m = StateT ScreenState m

renderPureScreen :: (MonadIO m) => PureScreen m a -> Ptr Word8 -> m a
renderPureScreen ps ptr = do
    (r, s) <- runStateT ps (ScreenState (DisplayList []) defaultColors)
    let screenWords = bitsToWords (s^.display.to (displaySprites %~ reverse))
        colorWords = calcColorTable (s^.colors)
    liftIO (screenToBytes4 screenWords colorWords ptr)
    return $! r

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
    fg c bi = colors.blocksSC.at bi %= defaultSet fgCB c

    -- bg :: Color -> BlockIndex -> PureScreen ()
    bg c bi = colors.blocksSC.at bi %= defaultSet bgCB c

    -- intensity :: Intensity -> BlockIndex -> PureScreen ()
    intensity i bi = colors.blocksSC.at bi %= defaultSet intensityCB i

    -- color :: ColorBlock -> BlockIndex -> PureScreen ()
    color cb bi = colors.blocksSC.at bi ?= cb

defaultSet len a mbCol = case mbCol of
    Nothing -> Just (setOn defaultColor)
    Just c  -> Just (setOn c)
  where
    setOn c = c & len .~ a
