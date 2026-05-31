-- | Board construction, queries, and win detection.
module Connect6.Board
  ( emptyBoard
  , inBounds
  , cellAt
  , isEmpty
  , placeStone
  , emptyPositions
  , occupiedPositions
  , directions
  , winningRunAt
  , isFull
  ) where

import Data.Array (array, bounds, (!), (//), assocs)
import Data.Ix (inRange)
import Connect6.Types

-- | Build an empty board of the given edge length.
emptyBoard :: Int -> Board
emptyBoard n = Board n cells
  where
    cells = array ((1, 1), (n, n))
              [ ((r, c), Empty) | r <- [1 .. n], c <- [1 .. n] ]

-- | Is a position on the board? Uses 'inRange' for proper per-axis box
-- containment (tuple @<=@ would compare lexicographically and wrongly admit
-- positions with a negative or oversized column).
inBounds :: Board -> Pos -> Bool
inBounds (Board _ cs) p = inRange (bounds cs) p

-- | Contents of a position. Off-board positions read as 'Empty'.
cellAt :: Board -> Pos -> Cell
cellAt b p
  | inBounds b p = boardCells b ! p
  | otherwise    = Empty

-- | Is the position empty (and on the board)?
isEmpty :: Board -> Pos -> Bool
isEmpty b p = inBounds b p && cellAt b p == Empty

-- | Place a stone, returning a new board. The caller must ensure the target is
-- a legal empty position; placing on an occupied or off-board cell is a no-op
-- so callers stay total.
placeStone :: Player -> Pos -> Board -> Board
placeStone pl p b
  | isEmpty b p = b { boardCells = boardCells b // [(p, Stone pl)] }
  | otherwise   = b

-- | All empty positions on the board.
emptyPositions :: Board -> [Pos]
emptyPositions b = [ p | (p, Empty) <- assocs (boardCells b) ]

-- | All occupied positions on the board.
occupiedPositions :: Board -> [Pos]
occupiedPositions b = [ p | (p, c) <- assocs (boardCells b), c /= Empty ]

-- | The four line directions (the reverse of each is covered by symmetry).
directions :: [(Int, Int)]
directions = [(0, 1), (1, 0), (1, 1), (1, -1)]

-- | If the stone just placed at @pos@ for @pl@ completes a winning run of
-- @winLen@ or more, return that run's positions; otherwise 'Nothing'.
winningRunAt :: Int -> Board -> Player -> Pos -> Maybe [Pos]
winningRunAt winLen b pl pos =
  case filter ((>= winLen) . length) runs of
    (r : _) -> Just r
    []      -> Nothing
  where
    runs = [ runThrough d | d <- directions ]
    runThrough (dr, dc) =
      let back = takeWhile mine (positionsFrom (negate dr, negate dc))
          fwd  = takeWhile mine (positionsFrom (dr, dc))
      in reverse back ++ [pos] ++ fwd
    positionsFrom (dr, dc) =
      [ (fst pos + k * dr, snd pos + k * dc) | k <- [1 .. boardSize b] ]
    mine p = inBounds b p && cellAt b p == Stone pl

-- | Are there no empty cells left?
isFull :: Board -> Bool
isFull = null . emptyPositions
