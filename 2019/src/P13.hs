{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module P13 where

import Control.Arrow
import Control.Monad ( forM_ )
import Control.Monad.Fail
import Data.Bifunctor
import Data.Functor.Identity
import Data.List.NonEmpty ( NonEmpty )
import qualified Data.List.NonEmpty as N
import qualified Data.Map as M

import ExceptT
import MonadIO
import StateT

import P5 hiding (main1, main2)

newtype Cabinet a =
  Cabinet { unCabinet :: ExceptT Status (StateT CabinetState Identity) a }
  deriving (Functor, Applicative, Monad)

instance MonadState Cabinet where
  type State Cabinet = CabinetState
  get = Cabinet get
  put = Cabinet . put

instance MonadExcept Cabinet where
  type Exc Cabinet = Status
  throwError = Cabinet . throwError
  catchError (Cabinet f) handle =
    Cabinet $ catchError f (unCabinet . handle)

instance MonadFail Cabinet where
  fail = error

data Tile = Empty | Wall | Block | Paddle | Ball
  deriving (Eq, Ord, Enum, Show, Read, Bounded)

tile2char :: Tile -> Char
tile2char = \case
  Empty -> ' '
  Wall -> '+'
  Ball -> '@'
  Paddle -> '='
  Block -> '#'

data VF a = V !a !a
  deriving (Functor, Ord, Eq)

instance Semigroup a => Semigroup (VF a) where
  (<>) = pairwise (<>)

instance Monoid a => Monoid (VF a) where
  mempty = V mempty mempty

pairwise :: (a -> a -> b) -> VF a -> VF a -> VF b
pairwise f (V x1 y1) (V x2 y2) = V (f x1 x2) (f y1 y2)

data Input = Neutral | TiltLeft | TiltRight

input2int :: Input -> Int
input2int = \case
  Neutral -> 0
  TiltLeft -> (-1)
  TiltRight -> 1

type V = VF Int

zero :: V
zero = V 0 0

type Screen = M.Map V Tile

data CabinetState =
  CabinetState
  { cpu :: CPU
  , screen :: Screen
  , score :: Int
  }

_cpu :: (CPU -> CPU) -> CabinetState -> CabinetState
_cpu f s = s { cpu = f (cpu s) }

_screen :: (Screen -> Screen) -> CabinetState -> CabinetState
_screen f s = s { screen = f (screen s) }

_score :: (Int -> Int) -> CabinetState -> CabinetState
_score f s = s { score = f (score s) }

runCabinet :: Cabinet a -> CabinetState -> (CabinetState, Either Status a)
runCabinet (Cabinet f) s = runIdentity $ unState (unExcept f) s

memset :: Addr -> Int -> Cabinet ()
memset a i = modify (_cpu (modifyMem (insert a i)))

initialState :: IO CabinetState
initialState = do
  cpu <- loadCPU "inputs/p13.txt"
  pure CabinetState
    { cpu, score = 0, screen = M.empty }

-- | Gets the next integer output from the CPU.
-- Returns Nothing if the CPU halts.
readCPU :: Cabinet Int
readCPU = do
  cpu <- gets cpu
  let (cpu', x) = runInterp pump cpu
  -- propagate exception to the manager
  x <- either throwError pure x
  modify (_cpu (const cpu'))
  pure x

putTile :: V -> Tile -> Cabinet ()
putTile v t =
  modify (_screen (M.insert v t))
  
drawScreen :: Cabinet ()
drawScreen = do
  x <- readCPU
  y <- readCPU
  t <- readCPU
  if (x, y) == (-1, 0) then
    modify (_score (const t))
  else
    putTile (V x y) (toEnum t)
  drawScreen

renderScreen :: Screen -> IO ()
renderScreen s = do
  let V xMax yMax = foldr (pairwise max) zero (M.keys s)
  forM_ [0..yMax] $ \y -> do
    putStrLn $ flip map [0..xMax] $ \x -> do
      case M.lookup (V x y) s of
        Nothing -> ' '
        Just t -> tile2char t

frame :: [CabinetState] -> IO ()
frame (c : cs) = do
  let (c', x) = runCabinet drawScreen c
  renderScreen (screen c')
  let find t = fst $ head $ filter (\(_, x) -> x == t) (M.assocs $ screen c')
  let V paddle _ = find Paddle
  let V ball _ = find Ball
  let pts = score c'
  case x of
    Left e -> case e of
      Blocked -> do
        putStrLn $ "Score: " ++ show pts
        case () of
          _ | ball < paddle -> frame (withInput TiltLeft c' : c : cs)
          _ | ball > paddle -> frame (withInput TiltRight c' : c : cs)
          _ -> frame (withInput Neutral c' : c : cs)
        -- putStr "Input: "
        -- getLine >>= \case
        --   "l" -> frame (withInput TiltLeft c' : c : cs)
        --   "r" -> frame (withInput TiltRight c' : c : cs)
        --   "" -> frame (withInput Neutral c' : c : cs)
        --   "b" -> frame cs
        --   _ -> frame (c : cs) -- bad input: restart
      Halt -> do
        putStrLn $ "Score: " ++ show pts
        putStrLn "Game Over! Turning back time."
        undefined
      NoOutput -> error "oh shit oh fuck"
    Right x -> frame (c' : cs)
  where
    withInput i c = c { cpu = _input' (const [input2int i]) (cpu c) }
      

main1 :: IO ()
main1 = do
  s <- initialState
  let (s', _) = runCabinet drawScreen s
  print $ length $ filter (== Block) $ M.elems $ screen s'

main2 :: IO ()
main2 = do
  s <- initialState
  let (s', _) = runCabinet (memset 0 2) s
  frame [s']
