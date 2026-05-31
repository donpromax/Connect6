-- | Pure rendering and key classification for the terminal UI.
--
-- All ANSI/escape handling lives here as 'String' transformations so the IO
-- loop in "Main" stays small. Colours use the 256-colour palette, which macOS
-- Terminal and iTerm2 both support.
module Tui
  ( Key(..)
  , classifyKey
  , render
  , moveCursor
  ) where

import Connect6.Types
import Connect6.Board (cellAt)

-- | A classified key press.
data Key = KUp | KDown | KLeft | KRight | KPlace | KQuit | KRestart | KOther
  deriving (Eq, Show)

-- | Classify a key from the raw characters read so far. The caller passes the
-- escape-sequence body for arrow keys (e.g. @\"[A\"@ after ESC).
classifyKey :: Char -> String -> Key
classifyKey c escSeq = case c of
  'q'    -> KQuit
  'Q'    -> KQuit
  'r'    -> KRestart
  'R'    -> KRestart
  ' '    -> KPlace
  '\n'   -> KPlace
  '\r'   -> KPlace
  'w'    -> KUp
  's'    -> KDown
  'a'    -> KLeft
  'd'    -> KRight
  'k'    -> KUp
  'j'    -> KDown
  'h'    -> KLeft
  'l'    -> KRight
  '\ESC' -> case escSeq of
              "[A" -> KUp
              "[B" -> KDown
              "[C" -> KRight
              "[D" -> KLeft
              _    -> KOther
  _      -> KOther

-- | Move the cursor one step, clamped to the board.
moveCursor :: Int -> Key -> Pos -> Pos
moveCursor n k (r, c) = clamp $ case k of
  KUp    -> (r - 1, c)
  KDown  -> (r + 1, c)
  KLeft  -> (r, c - 1)
  KRight -> (r, c + 1)
  _      -> (r, c)
  where clamp (a, b) = (max 1 (min n a), max 1 (min n b))

-- ANSI helpers ---------------------------------------------------------------

esc :: String -> String
esc s = "\ESC[" ++ s

reset :: String
reset = esc "0m"

-- | Set 256-colour background and foreground.
paint :: Int -> Int -> String
paint bg fg = esc ("48;5;" ++ show bg ++ ";38;5;" ++ show fg ++ "m")

woodBg, cursorBg, lastBg, emptyFg, blackFg, whiteFg :: Int
woodBg   = 180   -- tan board
cursorBg = 150   -- highlighted selection
lastBg   = 215   -- most-recent stones
emptyFg  = 94    -- faint grid dot
blackFg  = 16    -- black stone
whiteFg  = 231   -- white stone

-- | Right-justify into two columns.
pad2 :: String -> String
pad2 s = replicate (2 - length s) ' ' ++ s

-- Screen ---------------------------------------------------------------------

-- | Render the whole screen for the given board, cursor, and outcome.
render :: Player -> Maybe Outcome -> Pos -> GameState -> String
render human outcome cursor gs = concat
  [ esc "2J", esc "H"                           -- clear, home cursor
  , "  Connect6 / 六子棋\n\n"
  , header
  , concatMap rowLine [1 .. n]
  , "\n  " ++ statusLine human outcome gs ++ "\n"
  , "  Move: arrows / WASD / hjkl   Place: space   New game: r   Quit: q\n"
  ]
  where
    n         = boardSize board
    board     = gsBoard gs
    lastMoves = gsLastMoves gs

    header    = "   " ++ concatMap (pad2 . show) [1 .. n] ++ "\n"
    rowLine r = " " ++ pad2 (show r) ++ concatMap (cell r) [1 .. n] ++ "\n"

    cell r c =
      let pos = (r, c)
          bg | pos == cursor          = cursorBg
             | pos `elem` lastMoves    = lastBg
             | otherwise               = woodBg
          (fg, glyph) = case cellAt board pos of
                          Empty       -> (emptyFg, '.')
                          Stone Black -> (blackFg, '@')
                          Stone White -> (whiteFg, 'O')
      in paint bg fg ++ [glyph] ++ " " ++ reset

-- | One-line status: whose turn (and how many stones), or the result.
statusLine :: Player -> Maybe Outcome -> GameState -> String
statusLine human outcome gs = case outcome of
  Just (Won pl) -> show pl ++ tag pl ++ " wins!  Press r for a new game."
  Just Draw     -> "Draw - the board is full.  Press r for a new game."
  Nothing       -> show (gsToMove gs) ++ who
                     ++ " to move - " ++ show (gsRemaining gs) ++ " stone(s) this turn"
  where
    who    = if gsToMove gs == human then " (you)" else " (AI)"
    tag pl = if pl == human then " (you)" else " (AI)"
