-- | Graphical (window) front-end for Connect6, built on Gloss.
--
-- Reuses the gloss-free game core ("Connect6.Game", "Connect6.AI"): the world
-- is a 'GameState' plus an outcome and a hover cell, all handlers are pure, and
-- the AI is resolved synchronously after each human ply. Rendering uses the lit
-- stone / shadowed board primitives in "Draw".
module Main (main) where

import System.Environment (getArgs)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
  (play, Event(..), Key(..), KeyState(..), MouseButton(..))

import Connect6.Types
import Connect6.Game (initialState, applyMove, StepResult(..))
import Connect6.Engine (chooseMoves)
import Connect6.Board (isEmpty, occupiedPositions, cellAt)
import Layout
import Draw

-- | The full UI world.
data UI = UI
  { uiState  :: !GameState
  , uiOver   :: !(Maybe Outcome)
  , uiLayout :: !Layout
  , uiHover  :: !(Maybe Pos)
  }

main :: IO ()
main = do
  args <- getArgs
  let human = if any (`elem` ["--white", "-w", "white"]) args then White else Black
  play (InWindow "Connect6 / 六子棋" windowSize (60, 60))
       deskColor   -- window background (the table)
       30          -- frames per second
       (newGame human)
       drawUI
       onEvent
       (\_ w -> w)  -- no time-based stepping

-- | Start a fresh game, letting the AI open if it plays Black.
newGame :: Player -> UI
newGame human =
  let cfg = defaultConfig { configHuman = human }
  in settle (UI (initialState cfg) Nothing (mkLayout (configSize cfg)) Nothing)

humanOf :: UI -> Player
humanOf = configHuman . gsConfig . uiState

-- Event handling -------------------------------------------------------------

onEvent :: Event -> UI -> UI
onEvent (EventKey (MouseButton LeftButton) Down _ pt) ui = handleClick pt ui
onEvent (EventMotion pt) ui = ui { uiHover = pixelToCell (uiLayout ui) pt }
onEvent (EventKey (Char 'r') Down _ _) ui = (newGame (humanOf ui)) { uiHover = uiHover ui }
onEvent (EventKey (Char 'b') Down _ _) _  = newGame Black
onEvent (EventKey (Char 'w') Down _ _) _  = newGame White
onEvent _ ui = ui

-- | Place a human stone on a legal empty intersection, then let the AI reply.
handleClick :: (Float, Float) -> UI -> UI
handleClick pt ui
  | Just _ <- uiOver ui                          = ui
  | gsToMove (uiState ui) /= humanOf ui          = ui
  | Just pos <- pixelToCell (uiLayout ui) pt
  , isEmpty (gsBoard (uiState ui)) pos           = settle (applyOne pos ui)
  | otherwise                                    = ui

applyOne :: Pos -> UI -> UI
applyOne pos ui =
  case applyMove pos (uiState ui) of
    Finished gs o -> ui { uiState = gs, uiOver = Just o }
    Continue gs   -> ui { uiState = gs }

-- | Advance while it is the AI's turn until control returns or the game ends.
settle :: UI -> UI
settle ui
  | Just _ <- uiOver ui                  = ui
  | gsToMove (uiState ui) == humanOf ui  = ui
  | otherwise                            = settle (playAiTurn ui)

playAiTurn :: UI -> UI
playAiTurn ui = go (chooseMoves cfg (gsBoard gs) (gsToMove gs) (gsRemaining gs)) ui
  where
    gs  = uiState ui
    cfg = gsConfig gs
    go [] u = u
    go (p : ps) u = case uiOver u of
      Just _  -> u
      Nothing -> go ps (applyOne p u)

-- Rendering ------------------------------------------------------------------

drawUI :: UI -> Picture
drawUI ui = pictures [ translate 0 (layYShift l) boardGroup, statusPicture ui ]
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
  | Just _   <- uiOver ui                          = blank
  | gsToMove gs /= humanOf ui                       = blank
  | Just pos <- uiHover ui
  , isEmpty (gsBoard gs) pos                        =
      let (x, y) = cellCenter l pos
      in translate x y (ghostStone (stoneRadius l) (gsToMove gs))
  | otherwise                                       = blank
  where
    l  = uiLayout ui
    gs = uiState ui

-- | Status strip below the board: current turn / result, plus the controls.
statusPicture :: UI -> Picture
statusPicture ui = pictures
  [ label x0 (y0 + 24) 0.17 (statusText ui)
  , label x0  y0       0.11 "Click to place   |   R: restart   |   B/W: new game as Black/White"
  ]
  where
    x0 = negate (boardPx / 2) + 18
    y0 = negate (boardPx + statusH) / 2 + 16
    label x y s str = translate x y (scale s s (color white (text str)))

statusText :: UI -> String
statusText ui = case uiOver ui of
  Just (Won pl) -> show pl ++ tag pl ++ " wins!"
  Just Draw     -> "Draw - the board is full."
  Nothing       -> show (gsToMove gs) ++ who
                     ++ "  -  " ++ show (gsRemaining gs) ++ " stone(s) this turn"
  where
    gs     = uiState ui
    human  = humanOf ui
    who    = if gsToMove gs == human then " (you) to move" else " (AI) to move"
    tag pl = if pl == human then " (you)" else " (AI)"
