-- | Gloss drawing primitives for a polished, lit Connect6 board.
--
-- Stones are rendered as glossy spheres: an off-centre radial gradient (edge ->
-- highlight) shaded toward a fixed top-left light, a small specular dot, and a
-- soft drop shadow. The board has a bevelled wooden frame with faint grain.
module Draw
  ( deskColor
  , boardBackdrop
  , gridLines
  , starPoints
  , coordinates
  , litStone
  , ghostStone
  , lastMarker
  ) where

import Graphics.Gloss
import Connect6.Types
import Layout

-- Palette ---------------------------------------------------------------------

deskColor :: Color
deskColor = makeColorI 28 24 22 255          -- dark table behind the board

frameDark, frameLight, wood, woodGrain, gridColor, coordColor, glow :: Color
frameDark  = makeColorI 70  44  22 255        -- outer frame
frameLight = makeColorI 150 104 56 255        -- bevel highlight
wood       = makeColorI 214 170 110 255       -- board surface
woodGrain  = makeColorI 198 152 96  255       -- subtle grain
gridColor  = makeColorI 60  38  16 255        -- grid lines
coordColor = makeColorI 90  62  30 255        -- edge coordinates
glow       = makeColorI 220 60  50  255       -- last-move marker

-- Board -----------------------------------------------------------------------

-- | The wooden board: bevelled frame, surface, and faint horizontal grain.
boardBackdrop :: Layout -> Picture
boardBackdrop l = pictures
  [ color frameDark  (rectangleSolid s s)
  , color frameLight (translate (-2) 2 (rectangleSolid (s - 10) (s - 10)))  -- bevel
  , color wood       (rectangleSolid (s - 16) (s - 16))
  , pictures grain
  , vignette l
  ]
  where
    s     = boardPx
    half  = (s - 22) / 2
    grain = [ color woodGrain (line [(-half, y), (half, y)])
            | k <- [0 .. 23 :: Int]
            , let y = -half + fromIntegral k * (2 * half / 23) + 3 ]

-- | A soft darkening toward the board edges, faked with translucent rings.
vignette :: Layout -> Picture
vignette _ = pictures
  [ color (withAlpha a black) (thickCircle r t)
  | (r, t, a) <- [ (boardPx * 0.50, 70, 0.05)
                 , (boardPx * 0.56, 90, 0.10) ] ]

-- | The grid of intersection lines.
gridLines :: Layout -> Picture
gridLines l = color gridColor (pictures (hs ++ vs))
  where
    n  = layN l
    hs = [ line [cellCenter l (i, 1), cellCenter l (i, n)] | i <- [1 .. n] ]
    vs = [ line [cellCenter l (1, j), cellCenter l (n, j)] | j <- [1 .. n] ]

-- | The traditional star points (hoshi) on a 19x19 board.
starPoints :: Layout -> Picture
starPoints l = color gridColor (pictures [ dot p | p <- pts ])
  where
    pts | layN l == 19 = [ (r, c) | r <- [4, 10, 16], c <- [4, 10, 16] ]
        | otherwise    = []
    dot p = let (x, y) = cellCenter l p in translate x y (circleSolid 4)

-- | Row/column numbers just outside the grid on the top and left edges.
coordinates :: Layout -> Picture
coordinates l = color coordColor (pictures (cols ++ rows))
  where
    n    = layN l
    sp   = laySpacing l
    cols = [ let (x, y) = cellCenter l (1, c)
             in translate (x - 5) (y + sp * 0.55) (scale 0.09 0.09 (text (show c)))
           | c <- [1 .. n] ]
    rows = [ let (x, y) = cellCenter l (r, 1)
             in translate (x - sp * 0.95) (y - 5) (scale 0.09 0.09 (text (show r)))
           | r <- [1 .. n] ]

-- Stones ----------------------------------------------------------------------

-- | A glossy stone: drop shadow, shaded sphere, and specular highlight.
litStone :: Float -> Player -> Picture
litStone r pl = pictures [ softShadow r, sphere r edge hi, specular ]
  where
    (edge, hi, spec) = case pl of
      Black -> ( makeColor 0.02 0.02 0.04 1
               , makeColor 0.34 0.34 0.40 1
               , withAlpha 0.65 white )
      White -> ( makeColor 0.60 0.58 0.52 1
               , makeColor 1.00 0.99 0.96 1
               , withAlpha 0.90 white )
    specular = translate (-r * 0.30) (r * 0.34) (color spec (circleSolid (r * 0.16)))

-- | Off-centre radial gradient from @edge@ (rim) to @hi@ (lit centre), with the
-- highlight pushed toward the top-left light source.
sphere :: Float -> Color -> Color -> Picture
sphere r edge hi = pictures
  [ translate (negate d) d (color col (circleSolid rr))
  | k <- [0 .. steps]
  , let t   = fromIntegral k / fromIntegral steps   -- 0 = rim, 1 = centre
        rr  = r * (1 - 0.9 * t)
        d   = r * 0.22 * t
        col = mixColors (1 - t) t edge hi ]
  where steps = 28 :: Int

-- | A few stacked translucent discs, offset down-right, for a soft shadow.
softShadow :: Float -> Picture
softShadow r = pictures
  [ translate (r * 0.10) (negate (r * 0.12)) (color (withAlpha a black) (circleSolid (r * s)))
  | (a, s) <- [(0.06, 1.22), (0.10, 1.12), (0.16, 1.03)] ]

-- | A translucent preview stone shown under the cursor before placing.
ghostStone :: Float -> Player -> Picture
ghostStone r pl = pictures
  [ color (withAlpha 0.30 base) (circleSolid r)
  , color (withAlpha 0.55 base) (thickCircle r 2) ]
  where base = case pl of Black -> black; White -> greyN 0.95

-- | A glowing ring marking a stone played on the most recent turn.
lastMarker :: Float -> Picture
lastMarker r = pictures
  [ color (withAlpha 0.22 glow) (circleSolid (r * 0.5))
  , color glow (thickCircle (r * 0.42) 2.5) ]
