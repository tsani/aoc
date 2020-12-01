{-# LANGUAGE Strict #-}
{-# LANGUAGE ViewPatterns #-}

module P4 where

import Data.Word
import Data.List ( group )
import Prelude hiding ( (:) )

type D = Word8

data Password
  = PW
    { d1 :: !D
    , d2 :: !D
    , d3 :: !D
    , d4 :: !D
    , d5 :: !D
    , d6 :: !D
    }
  deriving (Eq, Ord)

instance Show Password where
  showsPrec _ (PW d1 d2 d3 d4 d5 d6) =
    shows d1 . shows d2 . shows d3 . shows d4 . shows d5 . shows d6

-- | Converts a password to a list, in LITTLE ENDIAN format!
toList :: Password -> [D]
toList (PW d1 d2 d3 d4 d5 d6) = [d6,d5,d4,d3,d2,d1]

fromList :: [D] -> Password
fromList [d6,d5,d4,d3,d2,d1] = PW d1 d2 d3 d4 d5 d6

lift :: ([D] -> [D]) -> Password -> Password
lift f = fromList . f . toList

liftF :: Functor f => ([D] -> f [D]) -> Password -> f Password
liftF f = fmap fromList . f . toList

tailZip :: [a] -> [(a, a)]
tailZip l = zip l (tail l)

hasDouble :: Password -> Bool
hasDouble = any (uncurry (==)) . tailZip . toList

hasDouble' :: Password -> Bool
hasDouble' = any (2 ==) . map length . group . toList

-- | Gets the next password.
-- Preserves the monotonicity invariant.
-- Will crash if all the digits end up used, i.e. the passwords
-- overflows its initial digit count.
inc :: Password -> Password
inc = snd . liftF go where
  go :: [D] -> (D, [D])
  go (9 : ds) = (d, d : ds') where
    (d, ds') = go ds
  go (d : ds) = (d + 1, d + 1 : ds)

-- | Naive increment: does not preserve monotonicity.
-- Crashes (inexhaustive match) if we overflow the digit count.
inc' :: Password -> Password
inc' = lift go where
  go (9 : ds) = 0 : go ds
  go (d : ds) = (d + 1) : ds


-- | Decides if a password meets the monotonicity requirement.
isMonotonic :: Password -> Bool
isMonotonic = go . toList where
  -- monotonicity is required in the big endian sense, so from a LE
  -- point of view, we need that each digit is *greater* than or equal
  -- to the next.
  go = all (uncurry (>=)) . tailZip

isValid :: Password -> Bool
isValid pw = isMonotonic pw && hasDouble pw

-- lower = head $ filter isValid $ iterate inc' $ fromList [8,2,3,2,0,4]
lower = PW 4 4 4 4 4 4
upper = fromList [7,4,2,4,6,8]

-- for part A
passwords1 = takeWhile (< upper) $ filter hasDouble $ iterate inc lower

-- for part B
passwords2 = takeWhile (< upper) $ filter hasDouble' $ iterate inc lower

main = print (length passwords2)
