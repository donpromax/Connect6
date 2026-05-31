-- | Terminal (full-screen) front-end for Connect6.
--
-- A colored, cursor-driven board rendered with ANSI escapes. Like the other
-- front-ends it reuses the gloss-free game core and resolves the AI's turn
-- synchronously after each human ply. Needs an interactive terminal; when stdin
-- is not a TTY it prints a single frame and exits so the build stays testable.
module Main (main) where

import System.Environment (getArgs)
import System.IO
import Control.Exception (finally)
import System.Posix.IO (stdInput)
import System.Posix.Terminal
  ( getTerminalAttributes, setTerminalAttributes
  , TerminalState(Immediately), TerminalMode(EnableEcho, ProcessInput)
  , withoutMode )

import Connect6.Types
import Connect6.Game (initialState, applyMove, StepResult(..))
import Connect6.Engine (chooseMoves)
import Connect6.Board (isEmpty)
import Tui

-- | Mutable-by-replacement UI state for the loop.
data UI = UI
  { uiState  :: !GameState
  , uiOver   :: !(Maybe Outcome)
  , uiCursor :: !Pos
  }

main :: IO ()
main = do
  args <- getArgs
  let human = if any (`elem` ["--white", "-w", "white"]) args then White else Black
      ui0   = settle (freshUI human)
  hSetBuffering stdout (BlockBuffering Nothing)
  tty <- hIsTerminalDevice stdin
  if not tty
    then do
      -- Non-interactive (e.g. piped/CI): render one frame and stop.
      putStr (frame ui0)
      hFlush stdout
      putStrLn "\n[connect6-tui needs an interactive terminal; run it directly.]"
    else withRawMode (loop ui0)

-- | Build a fresh game; if the AI plays Black it opens immediately (via 'settle').
freshUI :: Player -> UI
freshUI human =
  let cfg = defaultConfig { configHuman = human }
      mid = (configSize cfg + 1) `div` 2
  in UI (initialState cfg) Nothing (mid, mid)

humanOf :: UI -> Player
humanOf = configHuman . gsConfig . uiState

-- | The current screen for a UI.
frame :: UI -> String
frame ui = render (humanOf ui) (uiOver ui) (uiCursor ui) (uiState ui)

-- The interactive loop -------------------------------------------------------

loop :: UI -> IO ()
loop ui = do
  putStr (frame ui)
  hFlush stdout
  key <- readKeyIO
  case key of
    KQuit    -> return ()
    KRestart -> loop (settle (freshUI (humanOf ui)))
    _ | Just _ <- uiOver ui -> loop ui          -- game over: only r/q act
    KPlace   -> loop (settle (tryPlace ui))
    KOther   -> loop ui
    dir      -> loop ui { uiCursor = moveCursor (boardSize (gsBoard (uiState ui))) dir (uiCursor ui) }

-- | Place the human's stone under the cursor, if legal, then let the AI reply.
tryPlace :: UI -> UI
tryPlace ui
  | gsToMove (uiState ui) /= humanOf ui            = ui
  | isEmpty (gsBoard (uiState ui)) (uiCursor ui)   = applyOne (uiCursor ui) ui
  | otherwise                                      = ui

-- | Apply one validated stone for the player to move.
applyOne :: Pos -> UI -> UI
applyOne pos ui =
  case applyMove pos (uiState ui) of
    Finished gs o -> ui { uiState = gs, uiOver = Just o }
    Continue gs   -> ui { uiState = gs }

-- | Resolve AI turns until it is the human's move again or the game ends.
settle :: UI -> UI
settle ui
  | Just _ <- uiOver ui                  = ui
  | gsToMove (uiState ui) == humanOf ui  = ui
  | otherwise                            = settle (playAiTurn ui)

-- | Play every stone the AI is owed this turn.
playAiTurn :: UI -> UI
playAiTurn ui = go (chooseMoves cfg (gsBoard gs) (gsToMove gs) (gsRemaining gs)) ui
  where
    gs  = uiState ui
    cfg = gsConfig gs
    go [] u = u
    go (p : ps) u = case uiOver u of
      Just _  -> u
      Nothing -> go ps (applyOne p u)

-- Terminal handling ----------------------------------------------------------

-- | Read one key, decoding arrow-key escape sequences.
readKeyIO :: IO Key
readKeyIO = do
  c <- getChar
  if c == '\ESC'
    then do
      a <- getChar
      b <- getChar
      return (classifyKey c [a, b])
    else return (classifyKey c "")

-- | Run an action with the terminal in non-canonical, no-echo mode, restoring
-- the original settings (and showing the cursor) on exit. Uses the POSIX
-- terminal API directly so it works regardless of how stdin/stdout are wired.
withRawMode :: IO () -> IO ()
withRawMode act = do
  original <- getTerminalAttributes stdInput
  let raw = original `withoutMode` EnableEcho      -- no echo
                     `withoutMode` ProcessInput    -- non-canonical: char-at-a-time
  setTerminalAttributes stdInput raw Immediately
  hSetBuffering stdin NoBuffering
  putStr (esc "?25l")                              -- hide cursor
  hFlush stdout
  act `finally` restore original
  where
    esc s = "\ESC[" ++ s
    restore original = do
      setTerminalAttributes stdInput original Immediately
      putStr (esc "?25h" ++ esc "0m" ++ esc "2J" ++ esc "H")  -- show cursor, reset
      hFlush stdout
