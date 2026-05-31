-- | Terminal rendering of the board.
module Connect6.Render
  ( renderBoard
  , renderState
  , glyphFor
  ) where

import Connect6.Types
import Connect6.Board (cellAt)

-- | Single-character glyph for a cell. Black is @X@, White is @O@, empty is @.@.
glyphFor :: Cell -> Char
glyphFor Empty         = '.'
glyphFor (Stone Black) = 'X'
glyphFor (Stone White) = 'O'

-- | Right-justify a string into a field of the given width.
rightAlign :: Int -> String -> String
rightAlign w s = replicate (max 0 (w - length s)) ' ' ++ s

-- | Render a board as an aligned grid with 1-indexed row and column rulers.
-- Positions in @highlight@ (the most recent ply) are flagged with a @*@ so the
-- latest stones stand out.
renderBoard :: [Pos] -> Board -> String
renderBoard highlight b = unlines (header : rows)
  where
    n          = boardSize b
    labelW     = length (show n)        -- width of the row-label gutter
    cellW      = labelW + 1             -- width of each column field

    header     = rightAlign labelW "" ++ concatMap (rightAlign cellW . show) [1 .. n]
    rows       = [ rowLine r | r <- [1 .. n] ]
    rowLine r  = rightAlign labelW (show r) ++ concatMap (field r) [1 .. n]

    field r c  =
      let g = glyphFor (cellAt b (r, c))
      in if (r, c) `elem` highlight
           then rightAlign cellW ('*' : [g])   -- flag the most recent stones
           else rightAlign cellW [g]

-- | Render a full game state: the board plus a status line.
renderState :: GameState -> String
renderState gs =
  renderBoard (gsLastMoves gs) (gsBoard gs)
    ++ "\nTo move: " ++ show (gsToMove gs)
    ++ "  (" ++ show (gsRemaining gs) ++ " stone(s) this turn)\n"
