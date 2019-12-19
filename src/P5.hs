{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module P5 where

import Control.Monad ( forever )
import Data.Functor.Identity
import qualified Data.IntMap.Strict as M
import Data.List (intercalate)
import Control.Monad ( ap, when )
import Control.Monad.Fail
import Data.Foldable ( for_ )
import Data.Maybe (fromMaybe)
import Data.Traversable ( for )

import StateT
import ExceptT

type Mem = M.IntMap Int

type Program = [Instr]
data Instr
  = Done
  | Arith Op Param Param Param
  | Input Param
  | Output Param
  | Cond Cond Param Param Param
  | Jump Jump Param Param
  | Rel Param

data Cond = TLT | TEQ
data Jump = JNZ | JZE

cond2func = \case
  TLT -> (<)
  TEQ -> (==)

jump2func = \case
  JNZ -> (/= 0)
  JZE -> (== 0)

data Param = I Int | A Addr | R Addr

type Addr = Int
data Op = ADD | MUL

op2func = \case
  ADD -> (+)
  MUL -> (*)

prettyInstr :: Instr -> (String -> String)
prettyInstr = \case
  Done -> app "hlt"
  Arith op p1 p2 dst ->
    app (intercalate "\t" [op2mnemonic op, showParam p1, showParam p2, showParam dst])
  Input dst ->
    app $ "inp\t" ++ showParam dst
  Output p ->
    app $ "out\t" ++ showParam p
  Cond c p1 p2 dst ->
    app (intercalate "\t" [cond2mnemonic c, showParam p1, showParam p2, showParam dst])
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
decodeOp (split -> (m1:m2:m3:_, code)) = case code of
  99 -> (0, \[] -> Done)
  1 -> (3, \[p1, p2, p3] -> Arith ADD (mode m1 p1) (mode m2 p2) (mode m3 p3))
  2 -> (3, \[p1, p2, p3] -> Arith MUL (mode m1 p1) (mode m2 p2) (mode m3 p3))
  3 -> (1, \[p1] -> Input (mode m1 p1))
  4 -> (1, \[p] -> Output (mode m1 p))
  5 -> (2, \[p1, p2] -> Jump JNZ (mode m1 p1) (mode m2 p2))
  6 -> (2, \[p1, p2] -> Jump JZE (mode m1 p1) (mode m2 p2))
  7 -> (3, \[p1, p2, p3] -> Cond TLT (mode m1 p1) (mode m2 p2) (mode m3 p3))
  8 -> (3, \[p1, p2, p3] -> Cond TEQ (mode m1 p1) (mode m2 p2) (mode m3 p3))
  9 -> (1, \[p1] -> Rel (mode m1 p1))
  _ -> error $ "invalid opcode " ++ show code
  where
    mode 0 = A
    mode 1 = I
    mode 2 = R
    mode _ = error "invalid mode"

data CPU =
  CPU
  { pc :: !Int
  , mem :: !Mem
  , relBase :: !Int
  , _input :: [Int]
  , _output :: [Int]
  }
  deriving Show

_input' :: ([Int] -> [Int]) -> CPU -> CPU
_input' f CPU{..} = CPU { _input = f _input, .. }

_output' :: ([Int] -> [Int]) -> CPU -> CPU
_output' f CPU{..} = CPU { _output = f _output, .. }

modifyRelBase :: (Int -> Int) -> CPU -> CPU
modifyRelBase f CPU{..} = CPU { relBase = f relBase, .. }

modifyMem :: (Mem -> Mem) -> CPU -> CPU
modifyMem f CPU {..} = CPU { mem = f mem, .. }

modifyPc :: (Int -> Int) -> CPU -> CPU
modifyPc f CPU {..} = CPU {pc = f pc, .. }

insert :: Addr -> Int -> Mem -> Mem
insert = M.insert

input :: Interp Int
input = gets _input >>= \case
  [] -> throwError Blocked
  i : is -> do
    modify (_input' (const is))
    pure i

output :: Int -> Interp ()
output k = modify (_output' (++ [k]))

newtype Interp a =
  Interp { unInterp :: ExceptT Status (StateT CPU Identity) a }
  deriving (Functor, Applicative, Monad)

runInterp :: Interp a -> CPU -> (CPU, Either Status a)
runInterp (Interp f) s = runIdentity $ unState (unExcept f) s

instance MonadState Interp where
  type State Interp = CPU
  get = Interp get
  put = Interp . put

instance MonadExcept Interp where
  type Exc Interp = Status
  throwError = Interp . throwError
  catchError (Interp f) handle =
    Interp $ catchError f (unInterp . handle)

instance MonadFail Interp where
  fail = error

-- | Tries to extract a value from the output queue.
-- Throws an exception if there's no output.
getOutput :: Interp Int
getOutput = gets _output >>= \case
  [] -> throwError NoOutput
  x : _output -> do
    modify (_output' (const _output))
    pure x

getRelBase :: Interp Int
getRelBase = gets relBase

load :: Addr -> Interp (Maybe Int)
load i = gets (M.lookup i . mem)

load' :: Addr -> Interp Int
load' i = fromMaybe 0 <$> load i

store :: Addr -> Int -> Interp ()
store i x = modify (modifyMem (M.insert i x))

-- | Fetches the given number of entries from memory from the PC onwards.
fetch :: Int -> Interp [Int]
fetch = fetchOffset 0

fetchOffset :: Int -> Int -> Interp [Int]
fetchOffset off k = do
  pc <- gets pc
  for [off+pc..off+pc+k-1] load'

modifyPC :: (Int -> Int) -> Interp ()
modifyPC f = modify (modifyPc f)

-- | Sets the PC to the given number.
jumpTo :: Int -> Interp ()
jumpTo k = modifyPC (const k)

-- | Resolves a parameter that *must* be an address to an absolute
-- address.
resolve :: Param -> Interp Addr
resolve (A i) = pure i
resolve (R i) = (i+) <$> getRelBase
  
loadParam :: Param -> Interp Int
loadParam (I n) = pure n
loadParam x = load' =<< resolve x

-- | Decodes an opcode, fetches its parameters, and constructs the
-- instruction. Returns the number of parameters as well as the
-- instruction.
decode :: Int -> Interp (Int, Instr)
decode code = do
  let (k, instr) = decodeOp code
  i <- instr <$> fetchOffset 1 k
  pure (k, i)

data Status = Halt | Blocked | NoOutput
  deriving Show

-- | Returns true if a jump was executed. In this case, the PC should
-- not be updated.
execute :: Instr -> Interp Bool
execute Done = throwError Halt
execute (Arith (op2func -> op) p1 p2 dst) = do
  r1 <- loadParam p1
  r2 <- loadParam p2
  dst <- resolve dst
  store dst (r1 `op` r2)
  pure False
execute (Input dst) = do
  n <- input
  dst <- resolve dst
  store dst n
  pure False
execute (Output p) = do
  r <- loadParam p
  output r
  pure False
execute (Cond (cond2func -> op) p1 p2 dst) = do
  r1 <- loadParam p1
  r2 <- loadParam p2
  dst <- resolve dst
  store dst (fromEnum $ r1 `op` r2)
  pure False
execute (Jump (jump2func -> op) p1 p2) = do
  r <- loadParam p1
  dst <- loadParam p2
  if op r then
    jumpTo dst *> pure True
  else
    pure False
execute (Rel p) = do
  r <- loadParam p
  modify (modifyRelBase (+ r))
  pure False

-- | Run a fetch-decode-execute cycle.
step :: Interp ()
step = do
  (k, instr) <- decode . head =<< fetch 1
  execute instr >>= \case
    True -> pure ()
    False ->
      -- only modify the program counter after the execution of the
      -- instruction.  instruction execution may raise an exception.  Some
      -- exceptions are fatal, e.g. Halt but others are not, e.g. Blocked,
      -- and must be resolved by the OS before trying the instruction
      -- again.
      modifyPC (+ (k+1))

-- | Runs the interpreter until it generates an output,
pump :: Interp Int
pump = step *>
  getOutput `catchError` \case
    NoOutput -> pump
    e -> throwError e

-- | Pushes the given integer to the computer state's input queue and
-- pumps the computer for an output. Returns the new computer state
-- and the result, if any was generated before the computer encountered an error.
call' :: CPU -> [Int] -> (CPU, Either Status Int)
call' s x = runInterp pump s { _input = _input s ++ x }

-- | Pushes the given integers to the computer's input queue and pumps
-- the computer for the desired number of outputs.
-- Returns the generated outputs. If the length of the returned list
-- is shorter than the desired number of outputs, it's because the
-- computer halted.
call'' :: CPU -> [Int] -> Int -> (CPU, Either Status [Int])
call'' s ins k = runInterp (go k) s { _input = _input s ++ ins } where
  go 0 = pure []
  go k = do
    (pump >>= \x -> (x :) <$> go (k-1)) `catchError` \case
      Halt -> pure []
      e -> throwError e

loadCPU :: String -> IO CPU
loadCPU path = do
  mem <- foldr (uncurry M.insert) M.empty . zip [0..] . read . (++ "]") . ('[' :) <$> readFile path
  pure $ CPU { pc = 0, _input = [], _output = [], relBase = 0, .. }

run :: Int -> IO ()
run k = do
  s <- loadCPU "inputs/p5.txt"
  let (CPU {..}, x) = runInterp (forever step) s { _input = [k] }
  case x of
    Left e -> print e
    Right x -> x -- impossible
  for_ _output print

main1 = run 1

main2 = run 5
  
