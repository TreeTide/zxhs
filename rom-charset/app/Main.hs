module Main where

import Control.Exception (try, SomeException)
import qualified Data.ByteString as B
import Data.List (intersperse)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        romPath:_ -> do
            res <- try (B.readFile romPath)
            case res of
                Right content -> process content
                Left err -> putStrLn ("Error reading '"
                                      ++ romPath
                                      ++ "': "
                                      ++ show (err :: SomeException))
        _ -> putStrLn "Usage: rom-chars <48k.rom>"

-- | The ROM stores ASCII(ish) characters from 0x3D00, 8 bytes per character.
process :: B.ByteString -> IO ()
process rom = do
    let charData = B.drop 0x3D00 rom
        chars = B.unpack charData
        entries = go charNames chars
        names = map fst entries
    mapM_ (putStrLn . genConst) entries
    putStrLn "asciiPart :: [Sprite8]"
    putStrLn ("asciiPart = [" ++
              concat (intersperse "," . map genName $ names)
              ++ "]")
  where
    genName n = "char_" ++ n
    genConst (name, bytes) =
        let typez = genName name ++ " :: Sprite8"
            assign = genName name ++ " = sprite8 " ++ show bytes
        in typez ++ "\n" ++ assign ++ "\n"
    go (name:restNames) chars = case split8 chars of
        Just (charBytes, restBytes) ->
            (name, charBytes) : go restNames restBytes
        Nothing -> []
      where
        split8 :: [a] -> Maybe ([a], [a])
        split8 xs =
            let (as, bs) = splitAt 8 xs
            in if length as == 8 then Just (as, bs)
               else Nothing
    go _ _ = []

charNames :: [String]
charNames =
    [ "Space"
    , "Exclamation"
    , "Quote"
    , "Hash"
    , "Dollar"
    , "Percent"
    , "Amp"
    , "Apostrophe"
    , "OpenParen"
    , "CloseParen"
    , "Star"
    , "Plus"
    , "Comma"
    , "Dash"
    , "Dot"
    , "Slash"
    ] ++ map return "0123456789" ++
    [ "Colon"
    , "Semicolon"
    , "LeftBeak"
    , "Equal"
    , "RightBeak"
    , "Question"
    , "At"
    ] ++ map return "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ++
    [ "LeftBracket"
    , "Backslash"
    , "RightBracket"
    , "UpArrow"
    , "Underscore"
    , "Pound"
    ] ++ map return "abcdefghijklmnopqrstuvwxyz" ++
    [ "LeftBrace"
    , "Pipe"
    , "RightBrace"
    , "Tilde"
    , "Copyright"
    ]
