{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module Day7 (main) where

import Data.List (foldl')
import System.IO

type Size = Int

type Listing r = [(String, r)]

data FSF r
  = File Size
  | Dir (Listing r)
  deriving Functor

newtype Fix f = Fix { unFix :: f (Fix f) }

dir = Fix . Dir
file = Fix . File

type Alg f a = f a -> a

cata :: Functor f => Alg f a -> Fix f -> a
cata phi = phi . fmap (cata phi) . unFix

type FS = Fix FSF

data Cursor
  = Here -- ^ we are in a directory
    [(Listing FS, String, Listing FS)] -- ^ the entries in the parent directories
    FS -- ^ the content we're looking at

data CD = Root | Up | Down String

-- | Performs the given movement from the current position.
-- The target directory of a Down must already exist.
move :: CD -> Cursor -> Cursor
move Root d@(Here [] _) = d
move Root d = move Root $ move Up d
move Up (Here [] _) = error "can't cd up from root"
move Up (Here ((parentL, name, parentR) : parents) contents) =
  Here parents (dir $ parentL ++ (name, contents) : parentR)
move (Down dst) (Here parents contents) = Here ((l, name, r) : parents) x where
  (l, (name, x) : r) = break ((dst ==) . fst) $ case unFix contents of
    File _ -> error "can't cd down from a file"
    Dir contents -> contents

-- | Create an entry at the current location
touch :: Cursor -> (String, FS) -> Cursor
touch d@(Here parents content) (name, entry) = case unFix content of
  Dir entries -> case lookup name entries of
    Just _ -> d -- duplicate entry; noop
    Nothing -> Here parents $ dir $ (name, entry) : entries
  File _ -> error "can't create an entry within a file"

here :: Cursor -> FS
here (Here _ contents) = contents

data Cmd
  = LS (Listing FS)
  | CD CD

type Program = [Cmd]

emptyCursor :: Cursor
emptyCursor = Here [] $ dir []

parse :: String -> Program
parse input = parseCommands $ lines input where
  parseCommands [] = []
  parseCommands (line : lines) = case words line of
    ["$", "ls"] -> parseOutput lines
    ["$", "cd", "/"] -> CD Root : parseCommands lines
    ["$", "cd", ".."] -> CD Up : parseCommands lines
    ["$", "cd", name] -> CD (Down name) : parseCommands lines
    l -> error $ "wat " ++ show l
    
  parseOutput lines =
    let (output, lines') = break ((== '$') . head) lines in
    LS (map parseOutputLine output) : parseCommands lines'

  parseOutputLine line = case words line of
    ["dir", name] -> (name, dir [])
    [size, name] -> (name, file $ read size)

-- | Constructs the tree by following the program.
run :: Cursor -> Program -> Cursor
run = foldl' interpret where
  interpret cursor cmd = case cmd of
    LS contents -> foldl' touch cursor contents
    CD dir -> move dir cursor

-- | Extracts the root node of the tree regardless of where we are in it.
fullTree :: Cursor -> FS
fullTree = here . move Root

-- | Gets the sizes of every directory in the tree. Also calculates the size of the whole tree.
dirSizes :: FS -> ([Size], Size)
dirSizes = cata phi where
  phi = \case
    File sz -> ([], sz)
    Dir entries ->
      let (ds, sz) = foldr (\(_, (dirs1, sz1)) (dirs2, sz2) -> (dirs1 ++ dirs2, sz1 + sz2)) ([], 0) entries in
      (sz : ds, sz)

-- | Calculates a list of the sizes of the directories within the given tree.
-- The extra size returned is the size of the given FS.
dirsWithSizeAtMost :: Size -> FS -> [Size]
dirsWithSizeAtMost limit = filter (< limit) . fst . dirSizes

answer1 = sum . dirsWithSizeAtMost 100000 . fullTree . run emptyCursor . parse

answer2 input = minimum . filter (\sz -> sz + freeSpace >= requiredSpace) $ sizes where
  (sizes, fullSize) = dirSizes . fullTree . run emptyCursor . parse $ input
  totalSpace = 70000000
  requiredSpace = 30000000
  freeSpace = totalSpace - fullSize

main :: IO ()
main = withFile "input/day7.txt" ReadMode $ \h ->
  print =<< answer2 <$> hGetContents h
