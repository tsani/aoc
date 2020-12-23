{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Day14 where

import Data.Bits
import Data.Char ( isDigit )
import Data.Coerce
import Data.List ( foldl' )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Word

p1 :: IO ()
p1 = print . common runOp1 . parse =<< T.readFile "input/day14.txt"

p2 :: IO ()
p2 = print . common runOp2 . parse =<< T.readFile "input/day14.txt"

common runOp = sum . map snd . M.toList . mem . run runOp

data Bit = X | Set Bool deriving Show

-- | Represented with the LSB as the first item in the list.
type RawMask = [Bit]
newtype AndMask = AndMask { unAndMask :: Word64 } deriving Show
newtype OrMask = OrMask { unOrMask :: Word64 } deriving Show
newtype V2Mask = V2Mask { unV2Mask :: RawMask } deriving Show

type Mask = (AndMask, OrMask)

compileAndMask :: RawMask -> AndMask
compileAndMask = AndMask . foldr f 0 . zip [0..] where
  f (_, Set False) = id
  f (k, _) = (2^k .|.)

compileOrMask :: RawMask -> OrMask
compileOrMask = OrMask . foldr f 0 . zip [0..] where
  f (k, Set True) = (2^k .|.)
  f _ = id

-- | A new interpretation of a bit, where X means unchanged.
newtype MaskBit = MaskBit Bit
type RawV2Mask = [MaskBit]

-- | Compiles the version 2 mask into a list of v1 masks to be applied
-- to memory addresses
compileV2Mask :: V2Mask -> [Mask]
compileV2Mask (V2Mask bs) = map compileMask . coerce $ go bs where
  go [] = [[]]
  go (Set True : bs) = (Set True :) <$> go bs
  go (Set False : bs) = (X :) <$> go bs
  go (X : bs) = [True, False] >>= f (go bs) where
    f r b = (Set b :) <$> r

  decode :: (Word64 -> RawMask) -> (Word64 -> Word64)
  decode f = sum . zipWith g [0..] . f where
    g :: Word64 -> Bit -> Word64
    g _ X = error "can't decode X"
    g _ (Set False) = 0
    g k (Set True) = 2^k

compileMask :: RawMask -> Mask
compileMask m = (compileAndMask m, compileOrMask m)

applyMask :: Mask -> Word64 -> Word64
applyMask (AndMask one, OrMask two) w = w .&. one .|. two

newtype Address = Address { unAddress :: Word64 } deriving (Eq, Ord, Show)

data Op
  = SetMask RawMask
  | SetMem Address Word64
  deriving Show

parse :: Text -> [Op]
parse = map parseLine . T.lines where
  parseLine :: Text -> Op
  parseLine (T.splitOn " " -> ws) = case ws of
    [mask, _, value] | mask == "mask" ->
      SetMask (parseMask $ T.unpack value)
    [T.filter isDigit -> addr, _, value] ->
      SetMem (Address $ readText addr) (readText value)
    _ -> error "impossible line"

parseMask :: String -> RawMask
parseMask s = foldl' f [] s where
  f r c = (: r) $ case c of
    'X' -> X
    '1' -> Set True
    '0' -> Set False
    _ -> error "impossible mask"

readText :: Read a => Text -> a
readText = read . T.unpack

data State a = State { mem :: Map Address Word64, mask :: a }
  deriving Show

runOp1 :: State Mask -> Op -> State Mask
runOp1 State {mem, mask} = \case
  SetMask mask -> State {mem, mask = compileMask mask}
  SetMem addr value -> State
    { mem = M.insert addr (mask `applyMask` value) mem
    , mask
    }

runOp2 :: State [Mask] -> Op -> State [Mask]
runOp2 State {mem, mask} = \case
  SetMask mask -> State {mem, mask = compileV2Mask (V2Mask mask)}
  SetMem (Address addr) value -> State
    { mem = foldr (\a -> M.insert a value) mem (map (Address . (`applyMask` addr)) mask)
    , mask
    }

run f = foldl' f State { mem, mask } where
  mem = M.empty
  mask = undefined
