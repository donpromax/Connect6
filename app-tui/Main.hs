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
  { uiState   :: !GameState
  , uiOver    :: !(Maybe Outcome)
  , uiCursor  :: !Pos
  , uiHistory :: ![GameState]   -- ^ past human-to-move states, for undo
  , uiHint    :: ![Pos]         -- ^ cells the Hard AI suggests (the hint)
  }

main :: IO ()
main = do
  args <- getArgs
  let human = if any (`elem` ["--white", "-w", "white"]) args then White else Black
      level | any (`elem` ["--easy"])   args = Easy
            | any (`elem` ["--hard"])   args = Hard
            | otherwise                      = Medium
      ui0   = settle (freshUI level human)
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
freshUI :: Level -> Player -> UI
freshUI level human =
  let cfg = defaultConfig { configHuman = human, configLevel = level }
      mid = (configSize cfg + 1) `div` 2
  in UI (initialState cfg) Nothing (mid, mid) [] []

humanOf :: UI -> Player
humanOf = configHuman . gsConfig . uiState

curLevel :: UI -> Level
curLevel = configLevel . gsConfig . uiState

-- | The current screen for a UI.
frame :: UI -> String
frame ui = render (uiHint ui) (humanOf ui) (uiOver ui) (uiCursor ui) (uiState ui)

-- The interactive loop -------------------------------------------------------

loop :: UI -> IO ()
loop ui = do
  putStr (frame ui)
  hFlush stdout
  key <- readKeyIO
  case key of
    KQuit     -> return ()
    KRestart  -> loop (settle (freshUI (curLevel ui) (humanOf ui)))
    KLevel l  -> loop (settle (freshUI l (humanOf ui)))   -- switch difficulty, new game
    KUndo     -> loop (undo ui)
    KHint     -> giveHint ui >>= loop
    _ | Just _ <- uiOver ui -> loop ui          -- game over: only r/q/level/undo act
    KPlace    -> loop (clearHint (settle (tryPlace ui)))
    KOther    -> loop ui
    dir       -> loop ui { uiCursor = moveCursor (boardSize (gsBoard (uiState ui))) dir (uiCursor ui) }

-- | Ask the Hard AI to suggest the human's move (blocks briefly while it
-- searches). Only meaningful on the human's turn.
giveHint :: UI -> IO UI
giveHint ui
  | Just _ <- uiOver ui                  = return ui
  | gsToMove (uiState ui) /= humanOf ui  = return ui
  | otherwise = do
      putStr (frame ui)                  -- redraw first so the brief pause is visible
      hFlush stdout
      let gs   = uiState ui
          hcfg = (gsConfig gs) { configLevel = Hard }
      return ui { uiHint = chooseMoves hcfg (gsBoard gs) (gsToMove gs) (gsRemaining gs) }

-- | Drop a displayed hint (a placement invalidates it).
clearHint :: UI -> UI
clearHint ui = ui { uiHint = [] }

-- | Revert to the previous human-to-move state, if any.
undo :: UI -> UI
undo ui = case uiHistory ui of
  (prev : rest) -> ui { uiState = prev, uiOver = Nothing, uiHistory = rest, uiHint = [] }
  []            -> ui

-- | Place the human's stone under the cursor, if legal, then let the AI reply.
-- Pushes the pre-turn state onto the history at the start of the human's turn.
tryPlace :: UI -> UI
tryPlace ui
  | gsToMove gs /= humanOf ui      = ui
  | isEmpty (gsBoard gs) cursor    = applyOne cursor (pushHistory ui)
  | otherwise                      = ui
  where
    gs     = uiState ui
    cursor = uiCursor ui
    pushHistory u
      | null (gsPlaced gs) = u { uiHistory = gs : uiHistory u }   -- start of turn
      | otherwise          = u

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
