module Game.Tiles where

import Data.Foldable (foldMap)
import Data.Traversable (traverse)
import Linear (V2(..), _x, _y, _yx)
import Linear.Affine (Point(P))
import ZX.Screen (Sprite8)

import Game.RawTiles

-- TODO move to some ZxHs utility.

-- | A drawable composed of multiple block-aligned sprites.
-- Note that only the composition is block-aligned, rendering need not happen
-- on block boundaries.
newtype MultiSprite = MultiSprite { rowMajor :: [Element] }

-- | A singe sprite of the 'MultiSprite', along with its block position.
data Element = Element
    { elemBlockPos :: !(Point V2 Int)
    , elemSprite   :: !Sprite8
    }

spritesOf :: MultiSprite -> [Sprite8]
spritesOf = map elemSprite . rowMajor

foldMulti :: (Monoid b) => MultiSprite -> (Element -> b) -> b
foldMulti (MultiSprite es) f = foldMap f es

-- | Note: can use the Const applicative for monoid-like operations, in case
-- parallel traversal is needed.
-- TODO add example.
traverseMulti :: (Applicative f) => MultiSprite -> (Element -> f a) -> f [a]
traverseMulti (MultiSprite es) f = traverse f es

-- | Returns a rectangular area of the tilemap.
-- Input and output are row-major. 
tileArea :: [[Sprite8]] -> Int -> Int -> Int -> Int -> MultiSprite
tileArea ts x y w h = MultiSprite $ do
    (rowSlice, relY) <- (take h . drop y $ ts) `zip` [0..]
    (a, relX)        <- (take w . drop x $ rowSlice) `zip` [0..]
    return $! Element (P (V2 relX relY)) a 

-- TODO bounds checking
tile :: [[Sprite8]] -> Int -> Int -> Sprite8
tile ts x y = case (head . rowMajor $ tileArea ts x y 1 1) of
    Element _ e -> e

trackH = tile tiles 0 2
trackV = tile tiles 1 2
[bendDR, bendDL, bendUR, bendUL] = spritesOf (tileArea tiles 2 2 2 2)
train = tileArea tiles 2 4 2 2
carry = tileArea tiles 0 4 2 2
steamSequence = spritesOf (tileArea tiles 0 6 {- 6 + 2 empty -} 8 1)
