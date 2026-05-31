-- | Graphical (window) front-end for Connect6, built on Gloss.
--
-- Uses 'playIO' so the AI can run in a background thread: a human stone is shown
-- immediately, the window stays responsive with an animated "AI is thinking…",
-- and stone placements play a sound. The AI search (a @safe@ FFI call in
-- "Connect6.Engine") runs via 'forkIO'; its result is handed back through an
-- 'MVar' that the step callback polls.
module Main (main) where

import System.Environment (getArgs)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar)
import Control.Exception (evaluate, try, SomeException)
import Data.List (foldl')
import Data.Maybe (isJust)
import System.Process (spawnProcess, ProcessHandle)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
  (playIO, Event(..), Key(..), KeyState(..), MouseButton(..))

import Connect6.Types
import Connect6.Game (initialState, applyMove, StepResult(..))
import Connect6.Engine (chooseMoves)
import Connect6.Board (isEmpty, occupiedPositions, cellAt)
import Layout
import Draw

-- | The full UI world.
data UI = UI
  { uiState    :: !GameState
  , uiOver     :: !(Maybe Outcome)
  , uiLayout   :: !Layout
  , uiHover    :: !(Maybe Pos)
  , uiHistory  :: ![GameState]        -- ^ past human-to-move states, for undo
  , uiThinking :: !(Maybe (MVar [Pos]))  -- ^ in-flight AI search, if any
  , uiAnim     :: !Float              -- ^ seconds elapsed (drives the spinner)
  }

-- macOS system sounds (silently ignored if @afplay@ is unavailable).
humanSound, aiSound, winSound :: FilePath
humanSound = "/System/Library/Sounds/Pop.aiff"
aiSound    = "/System/Library/Sounds/Tink.aiff"
winSound   = "/System/Library/Sounds/Glass.aiff"

main :: IO ()
main = do
  args <- getArgs
  let human = if any (`elem` ["--white", "-w", "white"]) args then White else Black
      level | any (`elem` ["--easy"]) args = Easy
            | any (`elem` ["--hard"]) args = Hard
            | otherwise                    = Medium
  playIO (InWindow "Connect6 / 六子棋" windowSize (60, 60))
         deskColor
         30
         (newGame level human)
         drawUI
         handleEvent
         stepIO

-- | A fresh game (the AI's opening, if any, is driven by 'stepIO').
newGame :: Level -> Player -> UI
newGame level human =
  let cfg = defaultConfig { configHuman = human, configLevel = level }
  in UI (initialState cfg) Nothing (mkLayout (configSize cfg)) Nothing [] Nothing 0

humanOf :: UI -> Player
humanOf = configHuman . gsConfig . uiState

curLevel :: UI -> Level
curLevel = configLevel . gsConfig . uiState

-- Sound ----------------------------------------------------------------------

-- | Fire-and-forget a system sound; never blocks and never fails the game.
playSound :: FilePath -> IO ()
playSound path = do
  _ <- try (spawnProcess "afplay" [path]) :: IO (Either SomeException ProcessHandle)
  return ()

-- Event handling -------------------------------------------------------------

handleEvent :: Event -> UI -> IO UI
handleEvent (EventKey (MouseButton LeftButton) Down _ pt) ui = handleClick pt ui
handleEvent (EventMotion pt) ui = pure ui { uiHover = pixelToCell (uiLayout ui) pt }
handleEvent (EventKey (Char 'r') Down _ _) ui = pure (newGame (curLevel ui) (humanOf ui))
handleEvent (EventKey (Char 'b') Down _ _) _  = pure (newGame Medium Black)
handleEvent (EventKey (Char 'w') Down _ _) _  = pure (newGame Medium White)
handleEvent (EventKey (Char 'u') Down _ _) ui = pure (if isJust (uiThinking ui) then ui else undo ui)
handleEvent (EventKey (Char '1') Down _ _) ui = pure (newGame Easy   (humanOf ui))
handleEvent (EventKey (Char '2') Down _ _) ui = pure (newGame Medium (humanOf ui))
handleEvent (EventKey (Char '3') Down _ _) ui = pure (newGame Hard   (humanOf ui))
handleEvent _ ui = pure ui

-- | Place a human stone (ignored while the AI is thinking or the game is over).
handleClick :: (Float, Float) -> UI -> IO UI
handleClick pt ui
  | isJust (uiThinking ui)              = pure ui
  | Just _ <- uiOver ui                 = pure ui
  | gsToMove (uiState ui) /= humanOf ui = pure ui
  | Just pos <- pixelToCell (uiLayout ui) pt
  , isEmpty (gsBoard (uiState ui)) pos  = applyOneIO humanSound pos (pushHistory ui)
  | otherwise                           = pure ui

-- | Snapshot the current state at the start of the human's turn.
pushHistory :: UI -> UI
pushHistory ui
  | null (gsPlaced (uiState ui)) = ui { uiHistory = uiState ui : uiHistory ui }
  | otherwise                    = ui

-- | Revert to the previous human-to-move state, if any.
undo :: UI -> UI
undo ui = case uiHistory ui of
  (prev : rest) -> ui { uiState = prev, uiOver = Nothing, uiHistory = rest }
  []            -> ui

-- | Place one stone, play its sound, and record the outcome.
applyOneIO :: FilePath -> Pos -> UI -> IO UI
applyOneIO sfx pos ui = do
  playSound sfx
  case applyMove pos (uiState ui) of
    Finished gs o -> playSound winSound >> pure ui { uiState = gs, uiOver = Just o }
    Continue gs   -> pure ui { uiState = gs }

-- | Per-frame driver: advances the AI's turn without blocking the UI.
stepIO :: Float -> UI -> IO UI
stepIO dt ui0 =
  let ui = ui0 { uiAnim = uiAnim ui0 + dt }
  in case uiOver ui of
       Just _ -> pure ui
       Nothing
         | gsToMove (uiState ui) == humanOf ui -> pure ui
         | otherwise -> case uiThinking ui of
             Nothing -> startThinking ui
             Just mv -> do
               res <- tryTakeMVar mv
               case res of
                 Nothing    -> pure ui                          -- still searching
                 Just picks -> applyPicks picks ui { uiThinking = Nothing }

-- | Fork the AI search; its (forced) result lands in a fresh 'MVar'.
startThinking :: UI -> IO UI
startThinking ui = do
  mv <- newEmptyMVar
  let gs = uiState ui
  _ <- forkIO $ do
         let picks = chooseMoves (gsConfig gs) (gsBoard gs) (gsToMove gs) (gsRemaining gs)
         _ <- evaluate (foldl' (\a (r, c) -> a + r + c) 0 picks)   -- force to NF
         putMVar mv picks
  pure ui { uiThinking = Just mv }

-- | Apply the AI's stones one at a time (with sound), stopping on a win.
applyPicks :: [Pos] -> UI -> IO UI
applyPicks [] ui = pure ui
applyPicks (p : ps) ui = case uiOver ui of
  Just _  -> pure ui
  Nothing -> applyOneIO aiSound p ui >>= applyPicks ps

-- Rendering ------------------------------------------------------------------

drawUI :: UI -> IO Picture
drawUI ui = pure $ pictures [ translate 0 (layYShift l) boardGroup, statusPicture ui ]
  where
    l     = uiLayout ui
    gs    = uiState ui
    board = gsBoard gs
    rad   = stoneRadius l
    at p  = let (x, y) = cellCenter l p in translate x y
    boardGroup = pictures
      [ boardBackdrop l
      , gridLines l
      , starPoints l
      , coordinates l
      , pictures [ at p (stoneFor (cellAt board p)) | p <- occupiedPositions board ]
      , pictures [ at p (lastMarker rad)            | p <- gsLastMoves gs ]
      , hoverGhost ui
      ]
    stoneFor (Stone pl) = litStone rad pl
    stoneFor Empty      = blank

-- | Translucent preview of the next stone under the cursor (human's turn only).
hoverGhost :: UI -> Picture
hoverGhost ui
  | isJust (uiThinking ui)                          = blank
  | Just _   <- uiOver ui                           = blank
  | gsToMove gs /= humanOf ui                        = blank
  | Just pos <- uiHover ui
  , isEmpty (gsBoard gs) pos                         =
      let (x, y) = cellCenter l pos
      in translate x y (ghostStone (stoneRadius l) (gsToMove gs))
  | otherwise                                        = blank
  where
    l  = uiLayout ui
    gs = uiState ui

-- | Status strip: three lines spaced ~30 px apart so the vector text never
-- overlaps (a Gloss glyph is ~100 units tall at scale 1).
statusPicture :: UI -> Picture
statusPicture ui = pictures
  [ label x0 (y0 + 60) 0.15 (statusText ui)
  , label x0 (y0 + 30) 0.10 ("Difficulty: " ++ show (curLevel ui) ++ "   (1 easy / 2 medium / 3 hard)")
  , label x0  y0       0.10 "Click: place    U: undo    R: restart    B/W: play Black or White"
  ]
  where
    x0 = negate (boardPx / 2) + 18
    y0 = negate (boardPx + statusH) / 2 + 14
    label x y s str = translate x y (scale s s (color white (text str)))

statusText :: UI -> String
statusText ui
  | Just (Won pl) <- uiOver ui = show pl ++ tag pl ++ " wins!"
  | Just Draw     <- uiOver ui = "Draw - the board is full."
  | isJust (uiThinking ui)     = "AI is thinking" ++ replicate dots '.'
  | otherwise                  = show (gsToMove gs) ++ who
                                   ++ "  -  " ++ show (gsRemaining gs) ++ " stone(s) this turn"
  where
    gs    = uiState ui
    human = humanOf ui
    who    = if gsToMove gs == human then " (you) to move" else " (AI) to move"
    tag pl = if pl == human then " (you)" else " (AI)"
    dots   = 1 + (floor (uiAnim ui * 3) `mod` 3)
