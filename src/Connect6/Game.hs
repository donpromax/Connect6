-- | Game-state transitions and Connect6 turn rules.
--
-- Rules implemented: Black moves first and places a single stone on the opening
-- turn; thereafter each player places two stones per turn. A player wins the
-- instant they form a line of @winLen@ stones. A full board with no winner is a
-- draw.
module Connect6.Game
  ( initialState
  , turnSize
  , applyMove
  , StepResult(..)
  ) where

import Connect6.Types
import Connect6.Board

-- | The result of placing one stone.
data StepResult
  = Continue GameState          -- ^ Stone placed; play continues.
  | Finished GameState Outcome  -- ^ Stone placed and the game ended.

-- | The starting position: empty board, Black to move, one stone this turn.
initialState :: GameConfig -> GameState
initialState cfg = GameState
  { gsBoard     = emptyBoard (configSize cfg)
  , gsToMove    = Black
  , gsRemaining = 1            -- opening turn places a single stone
  , gsConfig    = cfg
  , gsPlaced    = []
  , gsLastMoves = []
  }

-- | How many stones a player places on a normal (non-opening) turn.
turnSize :: Int
turnSize = 2

-- | Place one stone for the player to move at @pos@. The caller is responsible
-- for validating that @pos@ is a legal empty cell (see "Connect6.Input").
applyMove :: Pos -> GameState -> StepResult
applyMove pos gs
  | Just _ <- winningRunAt winLen board' pl pos =
      Finished gs { gsBoard = board', gsPlaced = placed, gsLastMoves = placed } (Won pl)
  | remaining' > 0 =
      Continue gs { gsBoard = board', gsRemaining = remaining', gsPlaced = placed }
  | isFull board' =
      Finished (endTurn board' placed) Draw
  | otherwise =
      Continue (endTurn board' placed)
  where
    cfg        = gsConfig gs
    winLen     = configWinLen cfg
    pl         = gsToMove gs
    board'     = placeStone pl pos (gsBoard gs)
    placed     = gsPlaced gs ++ [pos]
    remaining' = gsRemaining gs - 1
    -- Hand the turn to the opponent, who then places a full turn's stones.
    endTurn b ps = gs
      { gsBoard     = b
      , gsToMove    = opponent pl
      , gsRemaining = turnSize
      , gsPlaced    = []
      , gsLastMoves = ps
      }
