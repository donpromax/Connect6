-- | Entry point: a CLI Connect6 game, human versus the C engine AI.
module Main (main) where

import System.IO (hFlush, hSetBuffering, stdout, BufferMode(NoBuffering))
import Connect6.Types
import Connect6.Game
import Connect6.Engine (chooseMoves)
import Connect6.Render (renderState)
import Connect6.Input (parsePos, validateMove)

-- | A history stack of past human-to-move states, for undo.
type History = [GameState]

-- | A parsed human command.
data Cmd = CmdUndo | CmdPlace Pos | CmdHint

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn banner
  human <- askSide
  level <- askLevel
  let cfg = defaultConfig { configHuman = human, configLevel = level }
  putStrLn ("\nYou are " ++ show human ++ " (" ++ glyph human ++ "), AI difficulty "
            ++ show level ++ ". Black places ONE stone on move one, then TWO per turn."
            ++ "\nType 'u' on your turn to undo your last move.\n")
  loop [] (initialState cfg)

-- | Drive one ply at a time until the game ends, threading the undo history.
loop :: History -> GameState -> IO ()
loop hist gs = do
  putStr (renderState gs)
  if gsToMove gs == configHuman (gsConfig gs)
    then humanTurn hist gs
    else aiTurn hist gs

-- | Run a single human stone placement, with undo support.
humanTurn :: History -> GameState -> IO ()
humanTurn hist gs = do
  cmd <- promptCmd gs
  case cmd of
    CmdUndo -> case hist of
      (prev : rest) -> putStrLn "  (undo)\n" >> loop rest prev
      []            -> putStrLn "  Nothing to undo." >> humanTurn hist gs
    CmdHint -> do
      putStrLn "  Thinking (Hard AI)..."
      let hcfg  = (gsConfig gs) { configLevel = Hard }
          picks = chooseMoves hcfg (gsBoard gs) (gsToMove gs) (gsRemaining gs)
      putStrLn ("  Hint: the Hard AI suggests "
                ++ unwords [ show r ++ " " ++ show c | (r, c) <- picks ])
      humanTurn hist gs
    CmdPlace pos ->
      let hist' = if null (gsPlaced gs) then gs : hist else hist  -- push at turn start
      in case applyMove pos gs of
           Finished gs' outcome -> finish gs' outcome
           Continue gs'
             | gsToMove gs' == gsToMove gs -> do        -- still placing the 2nd stone
                 putStr (renderState gs')
                 putStrLn "Place your second stone (or 'u' to undo)."
                 humanTurn hist' gs'
             | otherwise -> loop hist' gs'               -- turn handed over

-- | Run a single AI turn, reporting the stones it chose.
aiTurn :: History -> GameState -> IO ()
aiTurn hist gs = do
  putStrLn "AI is thinking..."
  playPicks hist gs (chooseMoves (gsConfig gs) (gsBoard gs) (gsToMove gs) (gsRemaining gs))

-- | Apply the AI's chosen stones one at a time.
playPicks :: History -> GameState -> [Pos] -> IO ()
playPicks hist gs [] = loop hist gs
playPicks hist gs (p : ps) = do
  putStrLn ("AI plays " ++ show p)
  case applyMove p gs of
    Finished gs' outcome -> finish gs' outcome
    Continue gs'         -> playPicks hist gs' ps

-- | Announce the result and stop.
finish :: GameState -> Outcome -> IO ()
finish gs outcome = do
  putStr (renderState gs)
  case outcome of
    Won pl -> putStrLn (show pl ++ " (" ++ glyph pl ++ ") wins! "
                        ++ if pl == configHuman (gsConfig gs) then "Congratulations!"
                                                              else "Better luck next time.")
    Draw   -> putStrLn "The board is full. It's a draw."

-- | Read a move or an undo command from the human.
promptCmd :: GameState -> IO Cmd
promptCmd gs = do
  putStr "Your move (row col; 'u' undo; 'h' hint): "
  hFlush stdout
  line <- getLine
  case lower (trim line) of
    "u"    -> return CmdUndo
    "undo" -> return CmdUndo
    "h"    -> return CmdHint
    "hint" -> return CmdHint
    "?"    -> return CmdHint
    _      -> case parsePos line >>= validateMove (gsBoard gs) of
                Right p  -> return (CmdPlace p)
                Left err -> putStrLn ("  " ++ err) >> promptCmd gs

-- | Ask which colour the human wants to play.
askSide :: IO Player
askSide = do
  putStr "Play as (B)lack-first or (W)hite-second? [B] "
  hFlush stdout
  line <- getLine
  case lower (trim line) of
    "w"     -> return White
    "white" -> return White
    _       -> return Black

-- | Ask the AI difficulty.
askLevel :: IO Level
askLevel = do
  putStr "Difficulty: (E)asy / (M)edium / (H)ard? [H] "
  hFlush stdout
  line <- getLine
  case lower (trim line) of
    "e"      -> return Easy
    "easy"   -> return Easy
    "m"      -> return Medium
    "medium" -> return Medium
    _        -> return Hard

-- | A short glyph label for a player.
glyph :: Player -> String
glyph Black = "X"
glyph White = "O"

lower :: String -> String
lower = map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)

trim :: String -> String
trim = f . f where f = reverse . dropWhile (== ' ')

banner :: String
banner = unlines
  [ "============================================"
  , "            Connect6  /  六子棋"
  , "  Line up six of your stones to win."
  , "============================================"
  ]
