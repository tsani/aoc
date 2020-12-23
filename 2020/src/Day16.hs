{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Day16 where

import Data.Bifunctor ( second )
import Data.List ( transpose, intercalate, sort, group, partition )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import Data.Maybe ( fromJust )
import Data.Set ( Set )
import qualified Data.Set as S
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Util ( readText, Range, inRange )

type FieldName = Text
type Ticket = [Int]
type Column = [Int]

-- | Maps the name of the field to the column number in the ticket table
type FieldMapping = [(FieldName, Int)]

-- | Maps the name of the field to all the columns it could be valid for
type FieldMultiMapping = [(FieldName, [Int])]

type FieldSpec = Map FieldName [Range]

data Problem = Problem { fields :: FieldSpec, yourTicket :: Ticket, otherTickets :: [Ticket] }

p1 :: IO ()
p1 = print . go1 . parse =<< T.readFile "input/day16.txt"

p2 :: IO ()
p2 = print . go2 . parse =<< T.readFile "input/day16.txt"

go1 Problem {fields, otherTickets} = sum . filter invalid $ concat otherTickets where
  invalid n = not (any (any (n `inRange`)) (snd <$> M.toList fields))

go2 p = product (map (\name -> yourTicket !! fieldIndex name) departureFields) where
  Problem {fields, yourTicket, otherTickets} = validTicketsOnly p

  fieldIndex = fromJust . (`lookup` findFieldMapping fields (yourTicket : otherTickets))

  departureFields :: [FieldName]
  departureFields = filter ("departure " `T.isPrefixOf`) . map fst $ M.toList fields

assert :: (a -> Bool) -> String -> a -> a
assert p msg x
  | p x = x
  | otherwise = error msg

toColumns :: [[a]] -> [(Int, [a])]
toColumns = zip [0..] . transpose

findFieldMapping :: FieldSpec -> [Ticket] -> FieldMapping
findFieldMapping (M.toList -> fields) (toColumns -> columns) = go fields columns where
  go _ [] = []
  go fields columns
    | length multi == 0 =
      error $ "nothing removed! Remaining columns: " ++ intercalate ", " (map (show . fst) columns)
    | otherwise = multi ++ go newFields newColumns
    where
    newFields = filter ((`notElem` deadNames) . fst) fields
    newColumns = filter ((`notElem` deadColumns) . fst) columns
    -- names
    (deadNames, deadColumns) = unzip multi
    -- all the unique mappings
    multi = map (second head) $ check $ keepUnique $ findAllFieldMappings fields columns where
      keepUnique = filter ((1 ==) . length . snd)
      check = assert (\l -> length l == length (group . sort . map snd $ l)) "can't decide"

  -- idea: for each field, find all the columns that could match that field
  -- there should be at least one field that matches exactly one column
  -- add that pair to the mapping, delete it from the inputs, and
  -- repeat until the input lists become empty.

findAllFieldMappings :: [(FieldName, [Range])] -> [(Int, Column)] -> FieldMultiMapping
findAllFieldMappings fields columns = map f fields where
  f :: (FieldName, [Range]) -> (FieldName, [Int])
  f (name, ranges) = (name, indices) where
    indices = case filter (all (\i -> any (i `inRange`) ranges) . snd) columns of
      [] -> error $ "no column matches for field " ++ T.unpack name
      ls -> map fst ls

-- | Removes all invalid other tickets from the problem.
validTicketsOnly :: Problem -> Problem
validTicketsOnly Problem {fields, yourTicket, otherTickets} = Problem
  { fields
  , yourTicket
  , otherTickets = filter valid otherTickets
  } where
  valid = all $ \i -> any (i `inRange`) (concat $ snd <$> M.toList fields)

parse :: Text -> Problem
parse (T.splitOn "\n\n" -> [rawFields, rawYourTicket, rawOtherTickets]) = Problem
  { fields = M.fromList (parseField <$> T.lines rawFields)
  , yourTicket = parseTicket (T.lines rawYourTicket !! 1)
  , otherTickets = parseTicket <$> tail (T.lines rawOtherTickets)
  }

parseTicket :: Text -> Ticket
parseTicket = map readText . T.splitOn ","

parseField :: Text -> (FieldName, [Range])
parseField (T.splitOn ": " -> [name, T.splitOn " or " -> map (T.splitOn "-") -> ranges]) =
  (name, map parseRange ranges) where
  parseRange = (\[one, two] -> (one, two)) . map readText
