{-# LANGUAGE TemplateHaskell #-}
module Game.Logic where

import Control.Lens
import Data.Bool (bool)
import qualified Data.Map.Strict as M
import Data.Map (Map)
import Linear (R2, V2(..), V3(..), _x, _y, _yx, _z)
import Linear.Affine (Point(P))
import Linear.Vector ((^*), (^/), (^+^))

data GS = GS
    { _rails :: Rails
    , _trains :: [Train]
    }

type Rails = Map TrackPos (Either Track Switch)

-- | Undirected track, the order of entry/exit directions doesn't matter.
-- TODO expose only smart ctor which guarantees internal sort, so foldTrackDir
--      would be simpler.
data Track = Track
    { _trackFrom :: Dir
    , _trackTo :: Dir
    }
    deriving (Eq, Ord, Show)

data Switch = Switch
    { _active :: Track
    , _rest :: [Track]
    , _letter :: Char
     -- ^ Switches are identified by letters on the game screen.
    }
    deriving (Eq, Ord, Show)

data Dir = L | R | U | D
    deriving (Eq, Ord, Show)

data Train = Train
    { _number :: Int
    , _engine :: Moving
    }
    deriving (Eq, Ord, Show)

-- A thing moving between the center of two edges of a 8x8 sized tile.
data Moving = Moving
    { _tilePos :: TrackPos
    , _movingOn :: Track
    , _movingForward :: Bool
      -- ^ Relative to the sorted track direction.
      --   TODO think about this a bit, doesn't sound elegant.
    , _progress :: Percent
      -- ^ Always increases from 0 to 100, while 'movingForward' tells the
      --   direction.
    }
    deriving (Eq, Ord, Show)

type TrackPos = Point V2 Int

-- | Between 0 and 100.
type Percent = Int

makeLenses ''GS
makeLenses ''Track
makeLenses ''Switch
makeLenses ''Moving
makeLenses ''Train

-- Above should be Types and below should be Logic.

-- | Safe matching on the pair of track directions (which are always disjoint).
foldTrackDir :: a -> a -> a -> a -> a -> a -> Track -> a
foldTrackDir lr lu ld ru rd ud (Track from to) = case sortedPair (from,to) of
    (L,R) -> lr
    (L,U) -> lu
    (L,D) -> ld
    (R,U) -> ru
    (R,D) -> rd
    (U,D) -> ud
    _     -> error "invalid sorted track direction pair"
  where 
    sortedPair (a,b) = if a < b then (a,b) else (b,a)

-- | Returns position within a virtual 1x1 square.
-- Caller should scale it to the actual block size.
positionInTile :: Moving -> Point V2 Float
positionInTile m =
    let (start, xOffsetByProgress, yOffsetByProgress) = foldTrackDir
            -- The starting position of the tile, and the progress-parametric
            -- paths for reaching the end position.
            (lp, id, const 0.0)
            (lp, half.cos.unorm, neg.half.sin.unorm)
            (lp, half.cos.unorm, half.sin.unorm)
            (rp, neg.half.cos.unorm, neg.half.sin.unorm)
            (rp, neg.half.cos.unorm, half.sin.unorm)
            (up, const 0.0, id)
            (m^.movingOn)
        p :: Float
        p = let f = bool (1.0 -) id (m^.movingForward)
            in f (fromIntegral (m^.progress) / 100.0)
    in start ^+^ P (V2 (xOffsetByProgress p) (yOffsetByProgress p))
  where
    pt x y = P (V2 x y)
    lp = pt 0.0 0.5
    rp = pt 1.0 0.5
    up = pt 0.5 0.0
    unorm = (* 3.14)
    half = (* 0.5)
    neg = (*(-1.0))
