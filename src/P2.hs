{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module P2 where

import qualified Data.IntMap.Strict as M
import Control.Monad ( ap )
import Control.Monad.Fail
import Data.Traversable ( for )

type Mem = M.IntMap Int

type Program = [Instr]
data Instr
  = Done
  | Instr Action

data Action =
  Action
  { op :: Op
  , src1 :: Addr
  , src2 :: Addr
  , dst :: Addr
  }

type Addr = Int
type Op = Int -> Int -> Int

decodeOp :: Int -> Maybe Op
decodeOp n = case n of
  1 -> Just addOp
  2 -> Just mulOp
  _ -> Nothing
  where
    addOp = (+)
    mulOp = (*)

-- | Decodes a list of exactly four integers.
decode :: [Maybe Int] -> Maybe Instr
decode (Just 99 : _) = Just Done
decode (sequence -> Just [o, src1, src2, dst]) = do
  op <- decodeOp o
  pure $ Instr Action {..}
decode _ = Nothing

data State =
  State
  { pc :: !Int
  , mem :: !Mem
  }

modifyMem :: (Mem -> Mem) -> State -> State
modifyMem f State {..} = State { mem = f mem, .. }

modifyPc :: (Int -> Int) -> State -> State
modifyPc f State {..} = State {pc = f pc, .. }

newtype Interp a =
  Interp { unInterp :: State -> (State, a) }
  deriving Functor

instance Applicative Interp where
  pure x = Interp $ \s -> (s, x)
  (<*>) = ap

instance Monad Interp where
  return = pure
  (Interp f) >>= k =
    Interp $ \s ->
    let (s', x) = f s in
      unInterp (k x) s'

instance MonadFail Interp where
  fail = error

gets :: (State -> s') -> Interp s'
gets f = Interp $ \s -> (s, f s)

modify :: (State -> State) -> Interp ()
modify f = Interp $ \s -> (f s, ())

load :: Addr -> Interp (Maybe Int)
load i = gets (M.lookup i . mem)

store :: Addr -> Int -> Interp ()
store i x = modify (modifyMem (M.insert i x))

-- | Loads four sequential integers from memory, starting at the
-- program counter, and increases the PC by 4.
fetch :: Interp [Maybe Int]
fetch = do
  pc <- gets pc
  modify (modifyPc (4+))
  for [pc..pc+3] load

data Status = Halt | Running

execute :: Instr -> Interp Status
execute Done = pure Halt
execute (Instr (Action {..})) = do
  Just r1 <- load src1
  Just r2 <- load src2
  store dst (op r1 r2)
  pure Running

-- | Runs the interpreter until it halts, and returns the value at
-- position 0.
trace :: Interp Int
trace = do
  fmap execute . decode <$> fetch >>= \case
    Nothing -> error "failed to do it"
    Just i -> i >>= \case
      Halt -> do
        Just x <- load 0
        pure x
      Running -> trace

loadInitialState :: IO State
loadInitialState = do
  mem <- foldr (uncurry M.insert) M.empty . zip [0..] . read . (++ "]") . ('[' :) <$> readFile "p2.txt"
  pure $ State { pc = 0, .. }

call :: State -> Int -> Int -> Int
call s a1 a2 = x where
  (_, x) = flip unInterp s $ do
    store 1 a1
    store 2 a2
    trace

mainPart1 :: IO ()
mainPart1 = do
  s <- loadInitialState
  print (call s 12 2)

target :: Int
target = 19690720

mainPart2 :: IO ()
mainPart2 = do
  s <- loadInitialState
  let (a1, a2) = head [ (a1, a2) | a1 <- [0..99], a2 <- [0..99], call s a1 a2 == target ]
  print (100 * a1 + a2)

main :: IO ()
main = mainPart2
