module Game.Tiles where

import Game.RawTiles

-- TODO move to some ZxHs utility.

tileArea :: [[a]] -> Int -> Int -> Int -> Int -> [[a]]
tileArea ts x y w h
    = map (take w . drop x)
    . take h . drop y
    $ ts

-- TODO bounds checking
tile :: [[a]] -> Int -> Int -> a
tile ts x y = (head . concat) (tileArea ts x y 1 1)

trackH = tile tiles 0 2
train = tileArea tiles 2 4 2 2
carry = tileArea tiles 0 4 2 2
steamSequence = concat (tileArea tiles 0 6 {- 6 + 2 empty -} 8 1)
