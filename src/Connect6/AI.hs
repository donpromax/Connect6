-- | A tactical Connect6 AI.
--
-- A full game-tree search is infeasible (a turn places two of ~360 stones), so
-- the AI combines hard forcing rules with a pruned one-turn search:
--
--   1. If a stone completes six, play it (win now).
--   2. Otherwise enumerate candidate /pairs/ of stones and score each by the
--      resulting board's threat balance minus the opponent's best reply. Because
--      a pair that leaves any opponent winning point scores terribly, the search
--      naturally blocks every immediate threat (using both stones when two must
--      be blocked) while still building its own double threats.
--
-- The window-based evaluation is split into offense and defense so the
-- opponent-reply term measures the opponent's /own/ attack, not its ability to
-- block us.
module Connect6.AI
  ( chooseMoves
  , scoreCell
  , candidatePositions
  , winsAt
  , winningPoints
  , evalBoard
  ) where

import Data.List (maximumBy, sortBy, tails, nub, foldl')
import Data.Ord (comparing, Down(..))
import Connect6.Types
import Connect6.Board

-- Tunables -------------------------------------------------------------------

-- | How many top-scoring cells seed the pair search.
candidateLimit :: Int
candidateLimit = 12

-- | Weight on the opponent's best reply (defensive lookahead). Large enough that
-- leaving the opponent a winning reply is never worth it.
replyBias :: Int
replyBias = 2

-- | Weight on the opponent's standing potential in the static evaluation.
defenseBias :: Int
defenseBias = 1

-- | Value of a pair that wins outright this turn; dominates everything else.
winBonus :: Int
winBonus = 1000000000

-- Window scoring -------------------------------------------------------------

-- | Score awarded to a clean length-@winLen@ window holding @n@ friendly
-- stones. Grows steeply so that completing or blocking a near-win dominates.
windowWeight :: Int -> Int -> Int
windowWeight winLen n
  | n <= 0          = 0
  | n >= winLen     = 10000000
  | n == winLen - 1 = 100000
  | n == winLen - 2 = 1000
  | n == winLen - 3 = 100
  | n == winLen - 4 = 10
  | otherwise       = 1

-- | The length-@winLen@ windows passing through @pos@ in all four directions.
windowsThrough :: GameConfig -> Board -> Pos -> [[Cell]]
windowsThrough cfg b pos =
  [ cells
  | (dr, dc) <- directions
  , start    <- [0 .. winLen - 1]
  , let coords = [ (r0 + (k - start) * dr, c0 + (k - start) * dc)
                 | k <- [0 .. winLen - 1] ]
  , all (inBounds b) coords
  , let cells = map (cellAt b) coords
  ]
  where
    winLen   = configWinLen cfg
    (r0, c0) = pos

count :: Cell -> [Cell] -> Int
count x = length . filter (== x)

-- | Friendly potential gained by placing one @pl@ stone on empty @pos@ (offense
-- only — windows already containing an opponent stone are dead and ignored).
offenseGain :: GameConfig -> Board -> Player -> Pos -> Int
offenseGain cfg b pl pos = sum (map gain (windowsThrough cfg b pos))
  where
    opp    = opponent pl
    winLen = configWinLen cfg
    gain w
      | count (Stone opp) w == 0 =
          let me = count (Stone pl) w
          in windowWeight winLen (me + 1) - windowWeight winLen me
      | otherwise = 0

-- | Heuristic value of a single cell for ranking candidates: friendly potential
-- gained plus opponent potential denied. (Kept for callers and tests.)
scoreCell :: GameConfig -> Board -> Player -> Pos -> Int
scoreCell cfg b pl pos = offenseGain cfg b pl pos + sum (map blockOf (windowsThrough cfg b pos))
  where
    opp    = opponent pl
    winLen = configWinLen cfg
    blockOf w
      | count (Stone pl) w == 0 = windowWeight winLen (count (Stone opp) w)
      | otherwise               = 0

-- Whole-board evaluation -----------------------------------------------------

-- | Every length-@winLen@ window on the board, each listed exactly once.
allWindows :: GameConfig -> Board -> [[Cell]]
allWindows cfg b =
  [ map (cellAt b) coords
  | (dr, dc) <- directions
  , r <- [1 .. n], c <- [1 .. n]
  , let coords = [ (r + k * dr, c + k * dc) | k <- [0 .. winLen - 1] ]
  , inBounds b (last coords)
  ]
  where
    n      = boardSize b
    winLen = configWinLen cfg

-- | (Black potential, White potential): the summed weight of every window that
-- is still open for that colour.
rawScores :: GameConfig -> Board -> (Int, Int)
rawScores cfg b = foldl' acc (0, 0) (allWindows cfg b)
  where
    winLen = configWinLen cfg
    acc (sb, sw) w =
      let nb = count (Stone Black) w
          nw = count (Stone White) w
          sb' = if nw == 0 then sb + windowWeight winLen nb else sb
          sw' = if nb == 0 then sw + windowWeight winLen nw else sw
      in (sb', sw')

-- | Static board value from @pl@'s perspective: own potential minus a fraction
-- of the opponent's.
evalBoard :: GameConfig -> Board -> Player -> Int
evalBoard cfg b pl =
  let (sb, sw)       = rawScores cfg b
      (mine, theirs) = if pl == Black then (sb, sw) else (sw, sb)
  in mine - defenseBias * theirs

-- Threats --------------------------------------------------------------------

-- | Does placing a @pl@ stone on @pos@ complete a winning line?
winsAt :: GameConfig -> Board -> Player -> Pos -> Bool
winsAt cfg b pl pos =
  isEmpty b pos &&
  case winningRunAt (configWinLen cfg) (placeStone pl pos b) pl pos of
    Just _  -> True
    Nothing -> False

-- | Empty cells where @pl@ would immediately win by playing a single stone.
winningPoints :: GameConfig -> Board -> Player -> [Pos]
winningPoints cfg b pl = [ p | p <- candidatePositions b, winsAt cfg b pl p ]

-- | The opponent's strongest single-stone attack on a board (used for
-- defensive lookahead). Measures the opponent building its own line, so it
-- spikes whenever the opponent still has a winning point.
opponentThreat :: GameConfig -> Board -> Player -> Int
opponentThreat cfg b opp =
  maximum (0 : [ offenseGain cfg b opp p | p <- candidatePositions b ])

-- Candidates -----------------------------------------------------------------

-- | Empty cells worth considering: those near an existing stone (Chebyshev
-- distance @<= radius@). On an empty board, just the centre.
candidatePositions :: Board -> [Pos]
candidatePositions b
  | null occupied = [ (mid, mid) ]
  | otherwise     = nub [ p | s <- occupied, p <- neighbourhood s, isEmpty b p ]
  where
    occupied = occupiedPositions b
    mid      = (boardSize b + 1) `div` 2
    radius   = 2
    neighbourhood (r, c) =
      [ (r + dr, c + dc) | dr <- [-radius .. radius], dc <- [-radius .. radius] ]

-- Move selection -------------------------------------------------------------

-- | Choose the stones for one turn: a single stone on the opening ply, otherwise
-- a coordinated pair.
chooseMoves :: GameConfig -> Board -> Player -> Int -> [Pos]
chooseMoves cfg board me n
  | n <= 1    = [singleChoice]
  | otherwise = pairChoice
  where
    opp = opponent me
    cs  = candidatePositions board
    mid = (boardSize board + 1) `div` 2

    immediateWins = winningPoints cfg board me

    -- Opening / leftover single stone: win if possible, else best ranked cell.
    singleChoice
      | (w : _) <- immediateWins = w
      | null cs                  = (mid, mid)
      | otherwise                = maximumByKey (scoreCell cfg board me) cs

    -- Two stones.
    pairChoice
      | (w : _) <- immediateWins = [w, complement w]   -- win this turn
      | otherwise                = searchPair

    -- A safe, useful partner for an already-winning stone (the game ends on the
    -- winning stone, so this is only ever a placeholder, but must be legal).
    complement w =
      case filter (/= w) immediateWins of
        (w2 : _) -> w2
        []       -> case filter (/= w) cs of
                      []   -> w
                      rest -> maximumByKey (scoreCell cfg (placeStone me w board) me) rest

    -- Pruned pair search with one-ply opponent reply.
    searchPair =
      let ranked   = take candidateLimit (sortDescKey (scoreCell cfg board me) cs)
          mustStop = winningPoints cfg board opp        -- always weigh blocks
          pool     = nub (mustStop ++ ranked)
          pairs    = [ (a, b) | (a : rest) <- tails pool, b <- rest ]
      in case pairs of
           [] -> take 2 (cs ++ [(mid, mid)])
           _  -> let (a, b) = maximumByKey (pairValue cfg board me) pairs
                 in [a, b]

-- | Value of playing the pair @(a, b)@: win outright, or the resulting board's
-- threat balance minus the opponent's best reply.
pairValue :: GameConfig -> Board -> Player -> (Pos, Pos) -> Int
pairValue cfg board me (a, b)
  | winsAt cfg board me a || winsAt cfg afterA me b = winBonus
  | otherwise = evalBoard cfg afterB me - replyBias * opponentThreat cfg afterB opp
  where
    opp    = opponent me
    afterA = placeStone me a board
    afterB = placeStone me b afterA

-- Small helpers --------------------------------------------------------------

maximumByKey :: Ord k => (a -> k) -> [a] -> a
maximumByKey f = snd . maximumBy (comparing fst) . map (\x -> (f x, x))

sortDescKey :: Ord k => (a -> k) -> [a] -> [a]
sortDescKey f = map snd . sortBy (comparing (Down . fst)) . map (\x -> (f x, x))
