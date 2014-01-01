
module Main where

import Control.Monad
import Data.Array ((!), (//))
import qualified Data.Array as A
import qualified Data.Set as S

newtype Player = Player Bool deriving (Eq, Ord, Bounded, Read, Show)

opponent :: Player -> Player
opponent (Player p) = Player . not $ p

data BinType = Mancala | PlainBin deriving (Eq, Ord, Read, Show)

data BinIndex =
    Bin00 | Bin01 | Bin02 | Bin03 | Bin04 | Bin05 | Bin06
  | Bin07 | Bin08 | Bin09 | Bin10 | Bin11 | Bin12 | Bin13
  deriving (Eq, Ord, Enum, Bounded, A.Ix, Read, Show)

binIndices :: [BinIndex]
binIndices = [minBound .. maxBound]

data Stone = Stone Integer deriving (Eq, Ord, Read, Show)

type Stones = S.Set Stone

numberOfStones :: Integer
numberOfStones = 24

stones :: Stones
stones = S.fromList . map Stone $ [1 .. numberOfStones]

data Bin =
  Bin
  { binType   :: BinType
  , binPlayer :: Player
  , binStones :: Stones
  } deriving (Eq, Ord, Read, Show)

type Game = A.Array BinIndex Bin

validGame :: Game -> Bool
validGame g = allStones && noDuplicateStones
  where
    allStones = S.unions (map binStones . A.elems $ g) == stones
    noDuplicateStones = all (== True) $ do
      x <- binIndices
      y <- binIndices
      guard $ x /= y
      return . S.null $ S.intersection (binStones $ g ! x) (binStones $ g ! y)

data Move = Move Player BinIndex deriving (Eq, Ord, Read, Show)

legalMove :: Game -> Move -> Bool
legalMove g (Move p i) = binType == PlainBin && binPlayer == p && binHasStones
  where
    Bin{..} = g ! i
    binHasStones = not . S.null $ binStones

data MoveResult = NextTurn Player | Victory Player deriving (Eq, Ord, Read, Show)

victor :: Game -> Maybe Player
victor _ = undefined

move :: Game -> Move -> Maybe (Game, MoveResult)
move g m | not $ legalMove g m = Nothing
move g (Move p i) =
  fmap (g',) . maybe (Just . NextTurn $ p') (Just . Victory) . victor $ g'
  where
    (g', i') = rawMove g p i
    freeTurn = isFreeTurnBin g' p i'
    p' = if freeTurn then p else opponent p

isFreeTurnBin :: Game -> Player -> BinIndex -> Bool
isFreeTurnBin g p i = binType == Mancala && binPlayer == p
  where
    Bin{..} = g ! cycSucc i

cycSucc :: (Eq a, Enum a, Bounded a) => a -> a
cycSucc x = if x == maxBound then minBound else succ x

rawMove :: Game -> Player -> BinIndex -> (Game, BinIndex)
rawMove g p i = uncurry (capture p) . sow g' (cycSucc i) $ ss
  where
    fromBin = g ! i
    ss = binStones fromBin
    fromBin' = fromBin { binStones = S.empty }
    g' = g // [(i, fromBin')]

sow :: Game -> BinIndex -> Stones -> (Game, BinIndex)
sow = undefined

capture :: Player -> Game -> BinIndex -> (Game, BinIndex)
capture = undefined

main :: IO ()
main = undefined move

