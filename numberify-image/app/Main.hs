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

process :: Pic.DynamicImage -> IO ()
process dynImage = do
    let img = Pic.convertRGBA8 dynImage
        w = Pic.imageWidth img
        h = Pic.imageHeight img
    forM_ [0 .. (h `div` tileSize) - 1] $ \blockY ->
    	forM_ [0 .. (w `div` tileSize) - 1] $ \blockX ->
            printTile img blockY blockX

printTile :: Pic.Image Pic.PixelRGBA8 -> Int -> Int -> IO ()
printTile img blockY blockX = do
    let rows = map rowToNumber [0 .. tileSize - 1]
        varName = "block" ++ show blockY ++ "_" ++ show blockX
    putStrLn (varName ++ " :: Sprite8")
    putStrLn ("varName = sprite8 [" ++
              concat (intersperse "," . map show $ rows)
              ++ "]")
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
			     bool 0 1 (a < 10 || r > 10)
        in foldr (\pix existing -> 2*existing + pix) 0 values
