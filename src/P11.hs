{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module P11 where

import qualified P5 as I
import StateT

import Control.Monad ( forM_ )
import Data.Functor.Identity
import Data.Bool ( bool )
import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as N
import qualified Data.Map as M
import Data.Maybe ( fromJust )
import Data.Tuple ( swap )

import Prelude

data V = V !Int !Int
  deriving (Eq, Ord, Read, Show)

vx :: V -> Int
vx (V x _) = x

vy :: V -> Int
vy (V _ y) = y

(+!) :: V -> V -> V
V x1 y1 +! V x2 y2 = V (x1 + x2) (y1 + y2)

data Dir =
  GoUp | GoRight | GoDown | GoLeft
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

dir2displacement :: Dir -> V
dir2displacement = \case
  GoUp -> V 0 1
  GoDown -> V 0 (-1)
  GoLeft -> V (-1) 0
  GoRight -> V 1 0

data Paint = Black | White
  deriving (Eq, Ord, Show, Read, Bounded, Enum)
  
cw :: Dir -> Dir
ccw :: Dir -> Dir
(cw, ccw) = (cw', ccw') where
  pairs = zip dirs (tail dirs) where
    dirs = cycle [minBound..maxBound]

  cw' = fromJust . flip lookup pairs
  ccw' = fromJust . flip lookup (swap <$> pairs)

toRot 0 = ccw
toRot 1 = cw

type Grid = M.Map V (NonEmpty Paint)

applyPaintToGrid :: V -> Paint -> Grid -> Grid
applyPaintToGrid p paint = M.alter f p where
  f :: Maybe (NonEmpty Paint) -> Maybe (NonEmpty Paint)
  f Nothing = Just (paint :| [])
  f (Just ps) = Just (N.cons paint ps)

data RobotState =
  RobotState
  { robotDir :: Dir
  , robotComputer :: I.CPU
  , robotGrid :: Grid
  , robotPos :: V
  }

newtype Robot a =
  Robot { unRobot :: StateT RobotState Identity a }
  deriving (Functor, Applicative, Monad)

instance MonadState Robot where
  type State Robot = RobotState
  get = Robot get
  put x = Robot (put x)

(<#>) = flip (<$>)

-- | Observes the paint color panel under the robot.
observePanel :: Robot Paint
observePanel =
  Robot (M.lookup <$> gets robotPos <*> gets robotGrid) <#> \case
    Nothing -> Black
    Just (p :| _) -> p

-- | Applies paint to the panel the robot is currently over.
applyPaint :: Paint -> Robot ()
applyPaint paint = Robot $ do
  p <- gets robotPos
  modify (robotGrid' (applyPaintToGrid p paint))

-- is this what deriving lenses is for???
  
robotComputer' :: (I.CPU -> I.CPU) -> RobotState -> RobotState
robotComputer' f s = s { robotComputer = f (robotComputer s) }

robotGrid' :: (Grid -> Grid) -> RobotState -> RobotState
robotGrid' f s = s { robotGrid = f (robotGrid s) }

robotDir' :: (Dir -> Dir) -> RobotState -> RobotState
robotDir' f s = s { robotDir = f (robotDir s) }

robotPos' :: (V -> V) -> RobotState -> RobotState
robotPos' f s = s { robotPos = f (robotPos s) }

-- | Call the inner Intcode computer with the given number of inputs
-- and expected number of outputs.
-- Returns fewer than desired outputs if the computer halts.
call :: [Int] -> Int -> Robot [Int]
call ins k = Robot $ do
  s <- gets robotComputer
  let (s', Right outs) = I.call'' s ins k
  modify (robotComputer' (const s'))
  pure outs

advance :: Robot ()
advance = Robot $ do
  d <- dir2displacement <$> gets robotDir
  modify (robotPos' (d +!))


-- | Executes a step of the robot.
-- * Observes the paint under the robot.
-- * Feeds it to the computer.
-- * Obtains the color to repaint and the direction to rotate.
-- * Rotates the robot.
-- * Moves the robot forward by one.
-- Returns False if the robot's computer halts.
step :: Robot Bool
step = do
  paint <- observePanel 
  call [fromEnum paint] 2 >>= \case
    [] -> pure False
    [toEnum -> paint, toRot -> rotate] -> do
      applyPaint paint
      Robot $ modify (robotDir' rotate)
      advance
      pure True
    _ -> error "uh oh"

-- | Runs the robot until its internal computer halts.
run :: Robot ()
run = bool (pure ()) run =<< step

loadState :: String -> IO RobotState
loadState path = do
  robotComputer <- I.loadCPU path
  pure RobotState
    { robotComputer
    , robotDir = GoUp
    , robotGrid = M.empty
    , robotPos = V 0 0
    }

answer1 :: RobotState -> Int
answer1 rs = n where
  (rs', _) = runIdentity $ unState (unRobot run) rs
  n = M.size (robotGrid rs')

answer2 :: RobotState -> RobotState
answer2 = fst . runIdentity . unState (unRobot run) . robotGrid' (applyPaintToGrid (V 0 0) White)

drawGrid grid = do
  let ks = M.keys grid
      (xmin, xmax) = let xs = map vx ks in (minimum xs, maximum xs)
      (ymin, ymax) = let ys = map vy ks in (minimum ys, maximum ys)
  -- my robot is confused and thinks down is up, so the grid is upside down.
  -- to fix this, we just draw the grid upside down to reverse the effect.
  forM_ [ymax,ymax-1..ymin] $ \y ->
    putStrLn $
      flip map [xmin..xmax] $ \x ->
        case maybe Black N.head $ M.lookup (V x y) grid of
          Black -> ' '
          White -> '#'
  
main1 :: IO ()
main1 = print =<< answer1 <$> loadState "inputs/p11.txt"

main2 :: IO ()
main2 = drawGrid =<< robotGrid . answer2 <$> loadState "inputs/p11.txt"
