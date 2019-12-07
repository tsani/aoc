{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module P5 where

import qualified Data.IntMap.Strict as M
import Data.List (intercalate)
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
  | Cond Cond Param Param Addr
  | Jump Jump Param Param

data Cond = TLT | TEQ
data Jump = JNZ | JZE

cond2func = \case
  TLT -> (<)
  TEQ -> (==)

jump2func = \case
  JNZ -> (/= 0)
  JZE -> (== 0)

data Param = I Int | A Addr

type Addr = Int
data Op = ADD | MUL

op2func = \case
  ADD -> (+)
  MUL -> (*)

prettyInstr :: Instr -> (String -> String)
prettyInstr = \case
  Done -> app "hlt"
  Arith op p1 p2 dst ->
    app (intercalate "\t" [op2mnemonic op, showParam p1, showParam p2, showParam (A dst)])
  Input dst ->
    app $ "inp\t" ++ showParam (A dst)
  Output p ->
    app $ "out\t" ++ showParam p
  Cond c p1 p2 dst ->
    app (intercalate "\t" [cond2mnemonic c, showParam p1, showParam p2, showParam (A dst)])
  Jump j p1 p2 ->
    app (intercalate "\t" [jump2mnemonic j, showParam p1, showParam p2])
  where
    app s = (++ s)
    op2mnemonic = \case
      ADD -> "add"
      MUL -> "mul"
    cond2mnemonic = \case
      TEQ -> "teq"
      TLT -> "tlt"
    jump2mnemonic = \case
      JNZ -> "jnz"
      JZE -> "jze"
    showParam = \case
      I x -> show x
      A x -> "$" ++ show x

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
  1 -> (3, \[p1, p2, p3] -> Arith ADD (mode m1 p1) (mode m2 p2) p3)
  2 -> (3, \[p1, p2, p3] -> Arith MUL (mode m1 p1) (mode m2 p2) p3)
  3 -> (1, \[p] -> Input p)
  4 -> (1, \[p] -> Output (mode m1 p))
  5 -> (2, \[p1, p2] -> Jump JNZ (mode m1 p1) (mode m2 p2))
  6 -> (2, \[p1, p2] -> Jump JZE (mode m1 p1) (mode m2 p2))
  7 -> (3, \[p1, p2, p3] -> Cond TLT (mode m1 p1) (mode m2 p2) p3)
  8 -> (3, \[p1, p2, p3] -> Cond TEQ (mode m1 p1) (mode m2 p2) p3)
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

-- | Tries to extract a value from the output queue.
-- Returns Nothing if there's no output.
getOutput :: Interp (Maybe Int)
getOutput = Interp $ \State{..} ->
  case _output of
    [] -> (State{..}, Nothing)
    x : _output -> (State{..}, Just x)

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
execute (Arith (op2func -> op) p1 p2 dst) = do
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
execute (Cond (cond2func -> op) p1 p2 dst) = do
  r1 <- loadParam p1
  r2 <- loadParam p2
  store dst (fromEnum $ r1 `op` r2)
  pure Running
execute (Jump (jump2func -> op) p1 p2) = do
  r <- loadParam p1
  dst <- loadParam p2
  when (op r) $ jumpTo dst
  pure Running

-- | Run a fetch-decode-execute cycle.
step :: Interp Status
step = fetch 1 >>= decode . head >>= execute

-- | Runs the interpreter until it halts.
trace :: Interp ()
trace =
  step >>= \case
    Halt -> pure ()
    Running -> trace

-- | Runs the interpreter until it generates an output,
-- removing the output from the output queue.
-- Returns Nothing if the interpreter halts before generating output.
pump :: Interp (Maybe Int)
pump = step >>= \case
  Running -> maybe pump (pure . pure) =<< getOutput 
  Halt -> pure Nothing

loadState :: String -> IO State
loadState path = do
  mem <- foldr (uncurry M.insert) M.empty . zip [0..] . read . (++ "]") . ('[' :) <$> readFile path
  pure $ State { pc = 0, _input = [], _output = [], .. }

run :: Int -> IO ()
run k = do
  s <- loadState "inputs/p5.txt"
  let (State {..}, _) = unInterp trace s { _input = [k] }
  for_ _output print

main1 = run 1

main2 = run 5
  
