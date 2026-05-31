-- | Lightweight test suite (no external test framework needed).
--
-- Each check prints PASS/FAIL; any failure makes the suite exit non-zero so it
-- works as a cabal @exitcode-stdio@ test.
module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import Data.IORef
import Data.List (sort)
import Connect6.Types
import Connect6.Board
import Connect6.Game
import Connect6.AI (scoreCell, chooseMoves, winningPoints, winsAt)
import qualified Connect6.Engine as Engine

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let check name ok = do
        putStrLn ((if ok then "PASS  " else "FAIL  ") ++ name)
        if ok then return () else modifyIORef' failures (+ 1)

  -- A horizontal run of six is a win; five is not.
  check "horizontal six wins" (hasWin (lineBoard 6))
  check "horizontal five does not win" (not (hasWin (lineBoard 5)))

  -- Placement is immutable and lands where expected.
  let b1 = placeStone Black (10, 10) (emptyBoard 19)
  check "placeStone fills target" (cellAt b1 (10, 10) == Stone Black)
  check "placeStone leaves origin empty" (cellAt (emptyBoard 19) (10, 10) == Empty)
  check "placeStone on occupied is no-op"
        (cellAt (placeStone White (10, 10) b1) (10, 10) == Stone Black)

  -- Bounds checks.
  check "in bounds" (inBounds (emptyBoard 19) (1, 1) && inBounds (emptyBoard 19) (19, 19))
  check "out of bounds" (not (inBounds (emptyBoard 19) (0, 5)) && not (inBounds (emptyBoard 19) (20, 1)))

  -- Opening turn places exactly one stone, then play passes to White.
  let s0 = initialState defaultConfig
  check "opening turn size is one" (gsRemaining s0 == 1 && gsToMove s0 == Black)
  case applyMove (10, 10) s0 of
    Continue s1 -> do
      check "after opening stone, White to move" (gsToMove s1 == White)
      check "White's turn places two" (gsRemaining s1 == 2)
    Finished _ _ -> check "opening stone must not end game" False

  -- A normal turn requires two stones before passing.
  case stepThrough s0 [(10, 10), (1, 1), (1, 2)] of
    Just sN -> check "two-stone turn returns to Black" (gsToMove sN == Black)
    Nothing -> check "two-stone turn returns to Black" False

  -- The AI completes an immediate win when offered five in a row.
  let fiveBoard = foldr (placeStone Black) (emptyBoard 19)
                        [(10, c) | c <- [4 .. 8]]      -- Black at cols 4..8 on row 10
      pick = head (chooseMoves defaultConfig fiveBoard Black 1)
  check "AI completes a winning six"
        (pick == (10, 3) || pick == (10, 9))

  -- The AI blocks an opponent's open five rather than ignoring it.
  let oppFive = foldr (placeStone White) (emptyBoard 19)
                      [(5, c) | c <- [4 .. 8]]
      block = head (chooseMoves defaultConfig oppFive Black 1)
  check "AI blocks opponent's five"
        (block == (5, 3) || block == (5, 9))

  -- Scoring prefers a cell that extends a friendly line over an isolated one.
  let near = placeStone Black (10, 9) (emptyBoard 19)
  check "scoreCell rewards adjacency"
        (scoreCell defaultConfig near Black (10, 10)
           > scoreCell defaultConfig near Black (1, 1))

  -- winsAt detects a completing stone.
  let openFive = foldr (placeStone Black) (emptyBoard 19) [(10, c) | c <- [4 .. 8]]
  check "winsAt detects a winning completion"
        (winsAt defaultConfig openFive Black (10, 9)
           && not (winsAt defaultConfig openFive Black (1, 1)))

  -- On a 2-stone turn, the AI takes an immediate win when one exists.
  let firstPick = head (chooseMoves defaultConfig openFive Black 2)
  check "AI takes the win on a two-stone turn"
        (firstPick == (10, 3) || firstPick == (10, 9))

  -- Two separate single-point threats must both be blocked, which needs both
  -- stones. Left threat: White 4..8 on row 5 with col 3 already blocked -> only
  -- (5,9) wins. Right threat: White 4..8 on row 15 with col 9 blocked -> only
  -- (15,3) wins.
  let twoThreats = foldr place (emptyBoard 19)
                     (  [ (White, (5,  c)) | c <- [4 .. 8] ]
                     ++ [ (White, (15, c)) | c <- [4 .. 8] ]
                     ++ [ (Black, (5, 3)), (Black, (15, 9)) ])
      place (pl, p) bd = placeStone pl p bd
  check "winningPoints finds exactly the two threats"
        (sort (winningPoints defaultConfig twoThreats White) == [(5, 9), (15, 3)])
  check "AI blocks both threats with its two stones"
        (sort (chooseMoves defaultConfig twoThreats Black 2) == [(5, 9), (15, 3)])

  -- The C engine (used by the front-ends) must satisfy the same tactics.
  check "C engine takes an immediate win"
        (let p = head (Engine.chooseMoves defaultConfig openFive Black 2)
         in p == (10, 3) || p == (10, 9))
  check "C engine blocks both threats with two stones"
        (sort (Engine.chooseMoves defaultConfig twoThreats Black 2) == [(5, 9), (15, 3)])
  check "C engine returns legal empty cells"
        (let ps = Engine.chooseMoves defaultConfig twoThreats Black 2
         in length ps == 2 && all (isEmpty twoThreats) ps)

  n <- readIORef failures
  if n == 0
    then putStrLn "\nAll tests passed." >> exitSuccess
    else putStrLn ("\n" ++ show n ++ " test(s) failed.") >> exitFailure

-- | A board with a horizontal Black run of the given length on row 10.
lineBoard :: Int -> Board
lineBoard k = foldr (placeStone Black) (emptyBoard 19) [(10, c) | c <- [1 .. k]]

-- | Does the last stone of the row-10 run complete a win?
hasWin :: Board -> Bool
hasWin b =
  case [ c | c <- [1 .. 19], cellAt b (10, c) == Stone Black ] of
    cs@(_ : _) ->
      let lastC = maximum cs
      in case winningRunAt (configWinLen defaultConfig) b Black (10, lastC) of
           Just _  -> True
           Nothing -> False
    [] -> False

-- | Apply a sequence of stones, threading state and stopping if the game ends.
stepThrough :: GameState -> [Pos] -> Maybe GameState
stepThrough gs [] = Just gs
stepThrough gs (p : ps) =
  case applyMove p gs of
    Continue gs' -> stepThrough gs' ps
    Finished _ _ -> Nothing
