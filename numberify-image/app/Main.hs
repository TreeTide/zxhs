module Main where

import Control.Exception (try, SomeException)
import Control.Monad (forM_)
import Data.Bool (bool)
import Data.List (intersperse)
import System.Environment (getArgs)
import qualified Codec.Picture as Pic

main :: IO ()
main = do
    args <- getArgs
    case args of
        imagePath:_ -> do
            res <- try (Pic.readImage imagePath)
            case res of
                Right (Right dynImage) -> process dynImage
                Right (Left errMsg) -> printError imagePath errMsg
                Left err -> printError imagePath (show (err :: SomeException))
        _ -> printUsage
  where
    printError path message =
        putStrLn ("Error reading '" ++ path ++ "': " ++ message)
    --
    printUsage =
        putStrLn "Usage: numberify-image <path to image>"

tileSize :: Int
tileSize = 8

-- | Hacky way to generate source-embeddable image data.
-- Somewhat useful if you don't want the main binary to depend on any image
-- loading libs.
process :: Pic.DynamicImage -> IO ()
process dynImg = do
    let tiles = extractTiles dynImg
    putStrLn "tiles :: [[Sprite8]]"
    putStrLn ("tiles = [" ++ concat (intersperse "," . map showTileRow $ tiles) ++ "]")
  where
    showTileRow :: [Tile] -> String
    showTileRow ts = "[" ++ concat (intersperse "," . map showTile $ ts) ++ "]"
    showTile :: Tile -> String
    showTile t = "sprite8 [" ++ concat (intersperse "," . map show $ t) ++ "]"

-- TODO extract the converter into a reusable lib.

type Tile = [Int]

extractTiles :: Pic.DynamicImage -> [[Tile]]
extractTiles dynImage = do
    let img = Pic.convertRGBA8 dynImage
        w = Pic.imageWidth img
        h = Pic.imageHeight img
    for [0 .. (h `div` tileSize) - 1] $ \blockY ->
    	for [0 .. (w `div` tileSize) - 1] $ \blockX ->
            makeTile img blockY blockX

makeTile :: Pic.Image Pic.PixelRGBA8 -> Int -> Int -> Tile
makeTile img blockY blockX = map rowToNumber [0 .. tileSize - 1]
  where
    rowToNumber :: Int -> Int  -- Could be Word8 with 8-pixel tile.
    rowToNumber rowIndex =
        let y = blockY * tileSize + rowIndex
            xBase = blockX * tileSize
            values = map (toBinary . readPixel) [xBase .. xBase + tileSize - 1]
                       where
                         readPixel x = Pic.pixelAt img x y
                         -- Black-white image assumed, so only checking R instead any.
                         -- Black non-transparent pixel counts as 'on', otherwise 'off'.
			 toBinary (Pic.PixelRGBA8 r _ _ a) =
			     bool 1 0 (a < 10 || r > 10)
        in foldl (\existing pix -> 2*existing + pix) 0 values

for = flip map
