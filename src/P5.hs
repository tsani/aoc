{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module P5 where

import qualified Data.IntMap.Strict as M
import Control.Monad ( ap, when )
import Control.Monad.Fail
import Data.Foldable ( for_ )
import Data.Maybe (fromMaybe)
import Data.Traversable ( for )

type Mem = M.IntMap Int

type Program = [Instr]
data Instr
  = Done
  | Arith Op Param Param Addr
  | Input Addr
  | Output Param
  | Cond (Int -> Int -> Bool) Param Param Addr
  | Jump (Int -> Bool) Param Param

data Param = I Int | A Addr

type Addr = Int
type Op = Int -> Int -> Int

digits :: Int -> [Int]
digits n = r : digits q where
  (q, r) = n `quotRem` 10

split (flip quotRem 100 -> (modes, code)) = (digits modes, code)

-- | Decodes an opcode.
-- Returns the number of operands required and a function that takes
-- the loaded operands to construct the instruction.
decodeOp :: Int -> (Int, [Int] -> Instr)
decodeOp (split -> (m1:m2:_, code)) = case code of
  99 -> (0, \[] -> Done)
  1 -> (3, \[p1, p2, p3] -> Arith (+) (mode m1 p1) (mode m2 p2) p3)
  2 -> (3, \[p1, p2, p3] -> Arith (*) (mode m1 p1) (mode m2 p2) p3)
  3 -> (1, \[p] -> Input p)
  4 -> (1, \[p] -> Output (mode m1 p))
  5 -> (2, \[p1, p2] -> Jump (0 /=) (mode m1 p1) (mode m2 p2))
  6 -> (2, \[p1, p2] -> Jump (0 ==) (mode m1 p1) (mode m2 p2))
  7 -> (3, \[p1, p2, p3] -> Cond (<) (mode m1 p1) (mode m2 p2) p3)
  8 -> (3, \[p1, p2, p3] -> Cond (==) (mode m1 p1) (mode m2 p2) p3)
  _ -> error "invalid opcode"
  where
    mode 0 = A
    mode 1 = I
    mode _ = error "invalid mode"

data State =
  State
  { pc :: !Int
  , mem :: !Mem
  , _input :: [Int]
  , _output :: [Int]
  }

modifyMem :: (Mem -> Mem) -> State -> State
modifyMem f State {..} = State { mem = f mem, .. }

modifyPc :: (Int -> Int) -> State -> State
modifyPc f State {..} = State {pc = f pc, .. }

input :: Interp Int
input = Interp $ \State {..} ->
  let (i : is) = _input in
  (State {_input = is, ..}, i)

output :: Int -> Interp ()
output k = Interp $ \State {..} ->
  (State {_output = _output ++ [k], ..}, ())

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

load' :: Addr -> Interp Int
load' i = fromMaybe (error "load failed") <$> load i

store :: Addr -> Int -> Interp ()
store i x = modify (modifyMem (M.insert i x))

-- | Fetches the given number of entries from memory from the PC onwards.
fetch :: Int -> Interp [Int]
fetch k = do
  pc <- gets pc
  modifyPC (k+)
  for [pc..pc+k-1] load'

modifyPC :: (Int -> Int) -> Interp ()
modifyPC f = modify (modifyPc f)

-- | Sets the PC to the given number.
jumpTo :: Int -> Interp ()
jumpTo k = modifyPC (const k)

loadParam :: Param -> Interp Int
loadParam (I n) = pure n
loadParam (A i) = load' i

-- | Decodes an opcode, fetches its parameters, and constructs the
-- instruction.
decode :: Int -> Interp Instr
decode code = do
  let (k, instr) = decodeOp code
  instr <$> fetch k

data Status = Halt | Running

execute :: Instr -> Interp Status
execute Done = pure Halt
execute (Arith op p1 p2 dst) = do
  r1 <- loadParam p1
  r2 <- loadParam p2
  store dst (r1 `op` r2)
  pure Running
execute (Input a) = do
  n <- input
  store a n
  pure Running
execute (Output p) = do
  r <- loadParam p
  output r
  pure Running
execute (Cond op p1 p2 dst) = do
  r1 <- loadParam p1
  r2 <- loadParam p2
  store dst (fromEnum $ r1 `op` r2)
  pure Running
execute (Jump op p1 p2) = do
  r <- loadParam p1
  dst <- loadParam p2
  when (op r) $ jumpTo dst
  pure Running

-- | Runs the interpreter until it halts, and returns the value at
-- position 0.
trace :: Interp ()
trace = do
  fetch 1 >>= decode . head >>= execute >>= \case
    Halt -> pure ()
    Running -> trace

loadInitialState :: [Int] -> IO State
loadInitialState _input = do
  mem <- foldr (uncurry M.insert) M.empty . zip [0..] . read . (++ "]") . ('[' :) <$> readFile "inputs/p5.txt"
  pure $ State { pc = 0, _output = [], .. }

run :: Int -> IO ()
run k = do
  (State {..}, _) <- unInterp trace <$> loadInitialState [k]
  for_ _output print

main1 = run 1

main2 = run 5
  
