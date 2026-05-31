-- | Core domain types for Connect6.
--
-- The board is a square grid of cells. Each cell is either empty or holds a
-- stone belonging to one of the two players. All values here are immutable;
-- moves produce new boards rather than mutating existing ones.
module Connect6.Types
  ( Player(..)
  , Cell(..)
  , Pos
  , Board(..)
  , GameConfig(..)
  , GameState(..)
  , Outcome(..)
  , opponent
  , stoneOf
  , defaultConfig
  ) where

import Data.Array (Array)

-- | The two players. Black always moves first.
data Player = Black | White
  deriving (Eq, Show)

-- | Contents of a single board intersection.
data Cell = Empty | Stone Player
  deriving (Eq, Show)

-- | A board position as @(row, column)@, both 1-indexed.
type Pos = (Int, Int)

-- | Immutable game board: a square 'Array' indexed from @(1,1)@ to @(size,size)@.
data Board = Board
  { boardSize  :: !Int
  , boardCells :: !(Array Pos Cell)
  }

-- | Static rules for a game.
data GameConfig = GameConfig
  { configSize     :: !Int  -- ^ Board edge length (e.g. 19).
  , configWinLen   :: !Int  -- ^ Stones in a row needed to win (6 for Connect6).
  , configHuman    :: !Player
  }

-- | Full mutable-by-replacement game state threaded through the main loop.
data GameState = GameState
  { gsBoard     :: !Board
  , gsToMove    :: !Player    -- ^ Whose turn it is.
  , gsRemaining :: !Int       -- ^ Stones the current player must still place this turn.
  , gsConfig    :: !GameConfig
  , gsPlaced    :: ![Pos]     -- ^ Stones placed so far in the in-progress turn.
  , gsLastMoves :: ![Pos]     -- ^ Stones of the last completed turn (for highlighting).
  }

-- | Result of the game once it ends.
data Outcome = Won Player | Draw
  deriving (Eq, Show)

-- | The other player.
opponent :: Player -> Player
opponent Black = White
opponent White = Black

-- | The cell value representing a given player's stone.
stoneOf :: Player -> Cell
stoneOf = Stone

-- | Standard Connect6: 19x19 board, six in a row, human plays Black.
defaultConfig :: GameConfig
defaultConfig = GameConfig
  { configSize   = 19
  , configWinLen = 6
  , configHuman  = Black
  }
