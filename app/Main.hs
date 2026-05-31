-- | Entry point: a CLI Connect6 game, human versus a heuristic AI.
module Main (main) where

import System.IO (hFlush, hSetBuffering, stdout, BufferMode(NoBuffering))
import Connect6.Types
import Connect6.Game
import Connect6.Engine (chooseMoves)
import Connect6.Render (renderState)
import Connect6.Input (parsePos, validateMove)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn banner
  human <- askSide
  let cfg = defaultConfig { configHuman = human }
  putStrLn ("\nYou are " ++ show human ++ " (" ++ glyph human ++ "). "
            ++ "Black moves first and places ONE stone; every later turn places TWO.\n")
  loop (initialState cfg)

-- | Drive one ply at a time until the game ends.
loop :: GameState -> IO ()
loop gs = do
  putStr (renderState gs)
  let human = configHuman (gsConfig gs)
  if gsToMove gs == human
    then humanTurn gs
    else aiTurn gs

-- | Run a single human turn (one or two stones, depending on the rules).
humanTurn :: GameState -> IO ()
humanTurn gs = do
  pos <- promptMove gs
  case applyMove pos gs of
    Finished gs' outcome -> finish gs' outcome
    Continue gs'
      | gsToMove gs' == gsToMove gs -> do        -- same player still placing
          putStr (renderState gs')
          putStrLn "Place your second stone."
          humanTurn gs'
      | otherwise -> loop gs'                     -- turn handed over

-- | Run a single AI turn, reporting the stones it chose.
aiTurn :: GameState -> IO ()
aiTurn gs = do
  putStrLn "AI is thinking..."
  let picks = chooseMoves (gsConfig gs) (gsBoard gs) (gsToMove gs) (gsRemaining gs)
  playPicks gs picks

-- | Apply the AI's chosen stones one at a time.
playPicks :: GameState -> [Pos] -> IO ()
playPicks gs [] = loop gs
playPicks gs (p : ps) = do
  putStrLn ("AI plays " ++ show p)
  case applyMove p gs of
    Finished gs' outcome -> finish gs' outcome
    Continue gs'         -> playPicks gs' ps

-- | Announce the result and stop.
finish :: GameState -> Outcome -> IO ()
finish gs outcome = do
  putStr (renderState gs)
  case outcome of
    Won pl -> putStrLn (show pl ++ " (" ++ glyph pl ++ ") wins! "
                        ++ if pl == configHuman (gsConfig gs) then "Congratulations!"
                                                              else "Better luck next time.")
    Draw   -> putStrLn "The board is full. It's a draw."

-- | Read and validate a legal move from the human.
promptMove :: GameState -> IO Pos
promptMove gs = do
  putStr "Your move (row col): "
  hFlush stdout
  line <- getLine
  case parsePos line >>= validateMove (gsBoard gs) of
    Right p  -> return p
    Left err -> do
      putStrLn ("  " ++ err)
      promptMove gs

-- | Ask which colour the human wants to play.
askSide :: IO Player
askSide = do
  putStr "Play as (B)lack-first or (W)hite-second? [B] "
  hFlush stdout
  line <- getLine
  case map toLower' (trim line) of
    "w"     -> return White
    "white" -> return White
    _       -> return Black
  where
    toLower' c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c
    trim = f . f where f = reverse . dropWhile (== ' ')

-- | A short glyph label for a player.
glyph :: Player -> String
glyph Black = "X"
glyph White = "O"

banner :: String
banner = unlines
  [ "============================================"
  , "            Connect6  /  六子棋"
  , "  Line up six of your stones to win."
  , "============================================"
  ]
