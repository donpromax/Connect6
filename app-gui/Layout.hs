-- | Pixel geometry for the graphical board: where each intersection sits and
-- which intersection a mouse click maps to. Kept free of any Gloss types so the
-- coordinate maths stays simple and self-contained.
module Layout
  ( Layout(..)
  , mkLayout
  , cellCenter
  , pixelToCell
  , windowSize
  , boardPx
  , statusH
  , stoneRadius
  ) where

import Connect6.Types (Pos)

-- | Edge length of the square board drawing area, in pixels — the main
-- resolution knob. Larger means more pixels per stone (and Gloss tessellates
-- bigger circles with more segments, so they look smoother). The window is
-- @boardPx x (boardPx + statusH)@, so raise this if you have screen height to
-- spare and lower it if the window is too tall. 900 gives a ~21 px stone radius.
boardPx :: Float
boardPx = 900

-- | Height of the status strip below the board, in pixels. Tall enough for
-- three well-spaced Gloss text lines (which are ~100 units tall at scale 1).
statusH :: Float
statusH = 104

-- | Empty border between the board edge and the outermost grid line.
marginPx :: Float
marginPx = 34

-- | Derived layout for an @n x n@ board.
data Layout = Layout
  { layN       :: !Int    -- ^ Board edge length (intersections per side).
  , laySpacing :: !Float  -- ^ Pixel distance between adjacent intersections.
  , layHalf    :: !Float  -- ^ Half of 'boardPx'.
  , layYShift  :: !Float  -- ^ Upward shift of the board to free the status strip.
  }

-- | Build the layout for a board of edge length @n@.
mkLayout :: Int -> Layout
mkLayout n = Layout
  { layN       = n
  , laySpacing = (boardPx - 2 * marginPx) / fromIntegral (max 1 (n - 1))
  , layHalf    = boardPx / 2
  , layYShift  = statusH / 2
  }

-- | Radius to draw a stone with, given the layout.
stoneRadius :: Layout -> Float
stoneRadius l = laySpacing l * 0.46

-- | Board-local pixel centre of intersection @(row, col)@ (origin at board
-- centre, before the board is shifted up). Row 1 is at the top.
cellCenter :: Layout -> Pos -> (Float, Float)
cellCenter l (r, c) =
  ( negate (layHalf l) + marginPx + fromIntegral (c - 1) * laySpacing l
  ,         layHalf l  - marginPx - fromIntegral (r - 1) * laySpacing l )

-- | Map a window-space click (origin at window centre) to the nearest
-- intersection, or 'Nothing' if the click is off-grid or too far from any.
pixelToCell :: Layout -> (Float, Float) -> Maybe Pos
pixelToCell l (gx, gy)
  | r >= 1, r <= layN l, c >= 1, c <= layN l, near = Just pos
  | otherwise                                       = Nothing
  where
    xl       = gx
    yl       = gy - layYShift l                    -- undo the board shift
    c        = round ((xl + layHalf l - marginPx) / laySpacing l) + 1
    r        = round ((layHalf l - marginPx - yl) / laySpacing l) + 1
    pos      = (r, c)
    (cx, cy) = cellCenter l pos
    near     = (xl - cx) ** 2 + (yl - cy) ** 2 <= (laySpacing l * 0.5) ** 2

-- | Overall window size in pixels: the board plus the status strip.
windowSize :: (Int, Int)
windowSize = (round boardPx, round (boardPx + statusH))
