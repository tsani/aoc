{-# LANGUAGE ViewPatterns, NamedFieldPuns, TupleSections, BangPatterns #-}

module Day11 where

import Data.Bifunctor
import Data.List (foldl', scanl', sort, find, nub)
import qualified Data.Vector as V

-- We can't represent a stress level as a plain integer for part 2.
-- What do we need to do with stress levels?
-- A stress level needs to able to
-- 1. be squared
-- 2. add a nonnegative constant
-- 3. multiply by a nonnegative constant
-- 4. checked for divisibility with one of a fixed set of prime numbers
-- 5. divided by three (ugh)
-- Let's call that set of prime numbers P and let a certain one be P_i.
-- We could represent a stress level as a sequence of (q, r)_i,
-- where each q_i and r_i are the quotient and remainder in division by P_i.
-- In principle, we can recover the stress level as q_i P_i + r_i
-- and this value would be the same for all i.
-- With this representation, how can we implement the necessary operations?
-- 1. (P_i q_i + r_i)^2
--    = P_i^2 q_i^2 + 2 P_i q_i r_i + r_i^2
--    = P_i (P_i q_i^2 + 2 q_i r_i) + r_i^2
--    = P_i q_i (P_i q_i + 2 r_i) + r_i^2
--    then divide r_i^2 by P_i and increase the quotient accordingly.
-- 2. Just add the constant to r_i, divide by P_i and increase the quotient accordingly.
-- 3. (P_i q_i + r_i) * k = P_i (k * q_i) + k r_i
--    Just divide r_i by P_i and increase the quotient accordingly.
-- 4. This is trivial. Look up the representation corresponding to the given P_i
--    and see if the remainder is zero.
-- 5. This is tricky:
--    (P_i q_i + r_i) / k
--    = P_i q_i / k + r_i / k
--      let q_i = k q1 + r1
--    = P_i (k q1 + r1) / k + r_i / k
--    = P_i (q1 + (r1 / k)) + r_i / k
--    = P_i q1 + P_i r1 / k + r_i / k
--    = P_i q1 + (P_i r1 + r_i) / k

data StressLevelP = Level { pI :: Int, qI :: Int, rI :: Int }
  deriving Show

toIntP :: StressLevelP -> Int
toIntP Level { pI, qI, rI } = pI * qI + rI

-- | Represents a given number as a quotient and remainder in division by a given number.
stressLevelP :: Int -> Int -> StressLevelP
stressLevelP k pI = Level { pI, qI, rI } where
  (qI, rI) = k `divMod` pI

-- | Represents a given number as a sequence of quotients and remainders by a
-- given list of numbers.
stressLevel :: Int -> [Int] -> StressLevel
stressLevel k = map (stressLevelP k)

addP :: Int -> StressLevelP -> StressLevelP
addP k Level { pI, qI, rI } = Level { pI, qI = qI + q, rI = rI' } where
  (q, rI') = (rI + k) `divMod` pI

multiplyP :: Int -> StressLevelP -> StressLevelP
multiplyP k Level { pI, qI, rI } = Level { pI, qI = qI + k + q, rI = rI' } where
  (q, rI') = (rI * k) `divMod` pI
  
squareP :: StressLevelP -> StressLevelP
squareP Level { pI, qI, rI } = Level { pI, qI = qI * (pI * qI + 2 * rI) + q, rI = rI' } where
  (q, rI') = (rI * rI) `divMod` pI

dividedByP :: StressLevelP -> Int -> StressLevelP
dividedByP Level { pI, qI, rI } k = Level { pI, qI = q + q', rI = r' } where
  (q, r) = qI `divMod` k
  rI' = (pI * r + rI) `div` k
  (q', r') = rI' `divMod` pI

type StressLevel = [StressLevelP]

toInt :: StressLevel -> Int
toInt = check . nub . map toIntP where
  check [x] = x
  check _ = error "uh oh"

add :: Int -> StressLevel -> StressLevel
add k = map (addP k)

multiply :: Int -> StressLevel -> StressLevel
multiply k = map (multiplyP k)

square :: StressLevel -> StressLevel
square = map squareP

dividedBy :: StressLevel -> Int -> StressLevel
dividedBy lvl k = map (`dividedByP` k) lvl

divides :: Int -> StressLevel -> Bool
divides p lvl = case find ((p ==) . pI) lvl of
  Nothing -> error "oops"
  Just Level {rI} -> rI == 0
  
type MonkeyId = Int

type Env = [Monkey]

data Monkey = Monkey
  { stress :: StressLevel -> StressLevel
  , target :: StressLevel -> MonkeyId
  , divisor :: Int
  }

type Queue = [StressLevel]

data State = State
  { queues :: !(V.Vector Queue)
    -- ^ the items held by each monkey, represented by their stress level
  }
  deriving Show

initialState :: [Queue] -> State
initialState items = State { queues = V.fromList items }

parse :: String -> (Env, State) 
parse = initialStateAndEnv . parse' where
  initialStateAndEnv :: [(Monkey, Queue)] -> (Env, State)
  initialStateAndEnv = second initialState . unzip
  
  parse' :: String -> [(Monkey, Queue)]
  parse' input = result where
    result = go (lines input)

    divisors = map (divisor . fst) result

    go = uncurry next . break null

    next :: [String] -> [String] -> [(Monkey, Queue)]
    next blockLines remainingLines = parseBlock (map words blockLines) : case remainingLines of
      [] -> []
      _ : remainingLines -> go remainingLines

    parseBlock :: [[String]] -> (Monkey, Queue)
    parseBlock
      [ _ -- Monkey id
      , "Starting" : "items:" : items
      , [_, _, _, _, op, term] -- Operation: new = old op val
      , [_, _, _, read -> divisor] -- Test: divisible by divisor
      , [_, _, _, _, _, read -> dstTrue] -- if true: throw to monkey dstTrue
      , [_, _, _, _, _, read -> dstFalse] -- if false: throw to monkey dstFalse
      ] =
        ( Monkey { divisor, stress , target }
        , map ((`stressLevel` divisors) . read . filter (',' /=)) items
        ) where
        stress = case op of
          "+" -> add (read term)
          "*" -> case term of
            "old" -> square
            n -> multiply (read n)
          _ -> error "unknown operation"
        target = \lvl -> if divisor `divides` lvl then dstTrue else dstFalse

queue :: (V.Vector Queue -> (V.Vector Queue, a)) -> State -> (State, a)
queue f = first (\queues -> State { queues }) . f . queues

queue' :: (V.Vector Queue -> V.Vector Queue) -> State -> State
queue' f = fst . queue ((, ()) . f)

-- | Transforms the element of a vector at a given index, while also calculating
-- a result.
modifyAtV :: Int -> (a -> (a, b)) -> V.Vector a -> (V.Vector a, b)
modifyAtV i f v = (v V.// [(i, y)], r) where
  (y, r) = f (v V.! i)

modifyAtV' i f = fst . modifyAtV i ((, ()) . f)

-- | Pushes an item onto the queue of a given monkey.
pushTo :: MonkeyId -> StressLevel -> State -> State
pushTo i item = queue' (modifyAtV' i (++ [item]))

-- | Pops the next item from the given Monkey, if any.
popFrom :: MonkeyId -> State -> (State, Maybe StressLevel)
popFrom i = queue (modifyAtV i pop) where
  pop :: Queue -> (Queue, Maybe StressLevel)
  pop [] = ([], Nothing)
  pop (!x:xs) = (xs, Just x)

-- | Performs the action of a Monkey.
execute :: Int -> State -> (MonkeyId, Monkey) -> State
execute k s (i, m) = case popFrom i s of
  (s', Just lvl) ->
    let lvl' = stress m lvl `dividedBy` k in
    execute k (pushTo (target m lvl') lvl' s') (i, m)
  (s', Nothing) -> s'

-- | Computes a trace of performing the action of each monkey, in turn, on the
-- state.
run :: Int -> Env -> State -> [State]
run k ms s = scanl' (execute k) s mids where
  mids = cycle (zip [0..] ms)

-- answer1 :: String -> Int
answer rounds k = product . take 2 . reverse . sort . V.toList . uncurry go . parse where
  -- | From a given list of monkeys and initial state, calculate a vector
  -- mapping monkey IDs to the count of times that monkey inspects an item
  go :: Env -> State -> V.Vector Int
  go env s =
    foldl' count (V.fromList $ replicate n 0) . zip (cycle ids) $ (take (rounds * n) $ run k env s) where

    ids = [0..(n-1)]
    n = length env

    -- | The monkey inspects as many items are in its queue.
    count counts (mId, s) = modifyAtV' mId (\k -> length (queues s V.! mId) + k) counts

answer1 = answer 20 3
answer2 = answer 10000 1

main :: IO ()
main = print . answer2 =<< readFile "input/day11.txt"
