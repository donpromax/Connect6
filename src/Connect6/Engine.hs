{-# LANGUAGE ForeignFunctionInterface #-}

-- | Haskell binding to the C search engine in @cbits/engine.c@.
--
-- The C function is a pure function of its inputs (it allocates no global state
-- across calls), so it is exposed through 'unsafePerformIO' with the same
-- signature as the reference Haskell AI ('Connect6.AI.chooseMoves'). The board
-- is marshalled to a row-major @int8@ array; the returned cell indices are
-- decoded back to 1-indexed positions.
module Connect6.Engine
  ( chooseMoves
  ) where

import Data.Int (Int8)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Array (withArray, allocaArray, peekArray)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)

import Connect6.Types
import Connect6.Board (cellAt)

-- NOTE: imported as @safe@ (not @unsafe@) so a long search running in a forked
-- thread releases the RTS capability and does not block other Haskell threads
-- (e.g. the GUI's render loop). The per-call overhead is negligible here.
foreign import ccall safe "c6_choose_moves"
  c_choose_moves
    :: Ptr Int8   -- ^ board, row-major n*n (0 empty, 1 black, 2 white)
    -> CInt       -- ^ n
    -> CInt       -- ^ winlen
    -> CInt       -- ^ player (1 black, 2 white)
    -> CInt       -- ^ stones to place
    -> CInt       -- ^ level (0 easy, 1 medium, 2 hard)
    -> Ptr CInt   -- ^ out buffer (length >= stones)
    -> IO CInt    -- ^ number of moves written

-- | Choose the stones for one turn using the C engine. Same contract as
-- 'Connect6.AI.chooseMoves': returns @stones@ positions to place in order.
chooseMoves :: GameConfig -> Board -> Player -> Int -> [Pos]
chooseMoves cfg board pl stones = unsafePerformIO $
  withArray cells $ \bptr ->
    allocaArray stones $ \out -> do
      k <- c_choose_moves bptr (ci n) (ci (configWinLen cfg)) (ci (code pl))
                          (ci stones) (ci (levelCode (configLevel cfg))) out
      idxs <- peekArray (fromIntegral k) out
      pure (map decode idxs)
  where
    n        = boardSize board
    ci       = fromIntegral
    cells    = [ cellCode (cellAt board (r, c)) | r <- [1 .. n], c <- [1 .. n] ]
    decode i = let v = fromIntegral i in (v `div` n + 1, v `mod` n + 1)
{-# NOINLINE chooseMoves #-}

code :: Player -> Int
code Black = 1
code White = 2

cellCode :: Cell -> Int8
cellCode Empty         = 0
cellCode (Stone Black) = 1
cellCode (Stone White) = 2
