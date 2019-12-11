{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module P11 where

import qualified P5 as I
import State

import Control.Monad ( forM_ )
import Data.Bool ( bool )
import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as N
import qualified Data.Map as M
import Data.Maybe ( fromJust )
import Data.Tuple ( swap )

import Prelude hiding (Left, Right) -- yuck

data V = V !Int !Int
  deriving (Eq, Ord, Read, Show)

vx :: V -> Int
vx (V x _) = x

vy :: V -> Int
vy (V _ y) = y

(+!) :: V -> V -> V
V x1 y1 +! V x2 y2 = V (x1 + x2) (y1 + y2)

data Dir =
  Up | Right | Down | Left
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

dir2displacement :: Dir -> V
dir2displacement = \case
  Up -> V 0 1
  Down -> V 0 (-1)
  Left -> V (-1) 0
  Right -> V 1 0

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
  , robotComputer :: I.State
  , robotGrid :: Grid
  , robotPos :: V
  }

newtype Robot a =
  Robot { unRobot :: State RobotState a }
  deriving (Functor, Applicative, Monad)

(<#>) = flip (<$>)

-- | Observes the paint color panel under the robot.
observePanel :: Robot Paint
observePanel =
  lift (M.lookup <$> gets robotPos <*> gets robotGrid) <#> \case
    Nothing -> Black
    Just (p :| _) -> p

-- | Applies paint to the panel the robot is currently over.
applyPaint :: Paint -> Robot ()
applyPaint paint = lift $ do
  p <- gets robotPos
  modify (robotGrid' (applyPaintToGrid p paint))

lift :: State RobotState a -> Robot a
lift = Robot

-- is this what deriving lenses is for???
  
robotComputer' :: (I.State -> I.State) -> RobotState -> RobotState
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
call ins k = lift $ do
  s <- gets robotComputer
  let (s', outs) = I.call'' s ins k
  modify (robotComputer' (const s'))
  pure outs

advance :: Robot ()
advance = lift $ do
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
      lift $ modify (robotDir' rotate)
      advance
      pure True
    _ -> error "uh oh"

-- | Runs the robot until its internal computer halts.
run :: Robot ()
run = bool (pure ()) run =<< step

loadState :: String -> IO RobotState
loadState path = do
  robotComputer <- I.loadState path
  pure RobotState
    { robotComputer
    , robotDir = Up
    , robotGrid = M.empty
    , robotPos = V 0 0
    }

answer1 :: RobotState -> Int
answer1 rs = n where
  (rs', _) = unState (unRobot run) rs
  n = M.size (robotGrid rs')

answer2 :: RobotState -> RobotState
answer2 = fst . unState (unRobot run) . robotGrid' (applyPaintToGrid (V 0 0) White)

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
