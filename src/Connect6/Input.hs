-- | Parsing and validating human input.
--
-- A move is two integers, @row@ then @column@ (both 1-indexed), separated by
-- whitespace or a comma, e.g. @\"10 10\"@ or @\"7,12\"@.
module Connect6.Input
  ( parsePos
  , validateMove
  ) where

import Data.Char (isDigit)
import Connect6.Types
import Connect6.Board (inBounds, isEmpty)

-- | Parse a @"row col"@ string into a position. Returns a 'Left' message on
-- malformed input.
parsePos :: String -> Either String Pos
parsePos s =
  case words (map normalize s) of
    [a, b] | all isDigit a, not (null a)
           , all isDigit b, not (null b) -> Right (read a, read b)
    _ -> Left "Enter a move as two numbers: <row> <col>, e.g. 10 10"
  where
    normalize ',' = ' '
    normalize c   = c

-- | Confirm a parsed position is on the board and currently empty.
validateMove :: Board -> Pos -> Either String Pos
validateMove b p
  | not (inBounds b p) = Left ("Position " ++ show p ++ " is off the board.")
  | not (isEmpty b p)  = Left ("Position " ++ show p ++ " is already taken.")
  | otherwise          = Right p
