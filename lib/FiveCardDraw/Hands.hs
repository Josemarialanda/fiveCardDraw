module FiveCardDraw.Hands where

import           Data.Function      (on)
import           Data.List          (foldl', groupBy, nubBy, sort, sortBy)
import           Data.Ord           (comparing)
import           FiveCardDraw.Types (Card (..), Hand (..), HandRank (..),
                                     Rank (..), RankedHand (..), SeatHand (..))

-- | A 'RankGroup' is a group of cards with the same rank.
type RankGroup = Int

-- | Determine the `HandRank` of a collection of cards.
--   `HandRank` is one of the possible poker hands.
value :: [Card] -> (HandRank, [Card])
value hand = maybe (ifNotFlush hand) ifFlush (maybeFlush hand)

-- | Convert a 'Hand' to a list of 'Card's.
handToCards :: Hand -> [Card]
handToCards Hand{..} = [hand'card1, hand'card2, hand'card3, hand'card4, hand'card5]

-- | Convert a 'Hand' to a 'RankedHand'.
--  A 'RankedHand' is a 'Hand' with a 'HandRank'.
valueOfHand :: Hand -> RankedHand
valueOfHand hand = RankedHand{rankedHand'handRank = handRank, rankedHand'hand = hand}
  where
    (handRank, _) = value $ handToCards hand

-- If the hand is not a flush, check for groups of cards.
ifNotFlush :: [Card] -> (HandRank, [Card])
ifNotFlush hand = maybe (checkGroups hand) (Straight,) (maybeStraight hand)

-- | If the hand is a flush, check for a straight flush.
ifFlush :: [Card] -> (HandRank, [Card])
ifFlush hand =
  maybe (Flush, take 5 hand) (StraightFlush,) (maybeStraight hand)

-- | Try to form a flush from a collection of cards.
maybeFlush :: [Card] -> Maybe [Card]
maybeFlush cs
  | length cs' >= 5 = Just cs'
  | otherwise = Nothing
  where
    sortBySuit :: [Card] -> [Card]
    sortBySuit = sortBy (comparing card'suit <> flip compare)

    groupBySuit :: [Card] -> [[Card]]
    groupBySuit = groupBy ((==) `on` card'suit)

    cs' :: [Card]
    cs' = head $ sortByLength $ groupBySuit $ sortBySuit cs

-- | Try to form a straight from a collection of cards.
maybeStraight :: [Card] -> Maybe [Card]
maybeStraight cards
  | length cs'' >= 5 = Just (lastNelems 5 cs'')
  | otherwise = maybeWheel cardsUniqRanks
  where
    cardsUniqRanks :: [Card]
    cardsUniqRanks = nubBy ((==) `on` card'rank) cards

    cs'' :: [Card]
    cs'' = head $ sortByLength $ groupBySuccCards $ sort cardsUniqRanks

    lastNelems :: Int -> [a] -> [a]
    lastNelems n xs = foldl' (const . drop 1) xs (drop n xs)

    groupBySuccCards :: [Card] -> [[Card]]
    groupBySuccCards = foldr f []
      where
        f :: Card -> [[Card]] -> [[Card]]
        f a [] = [[a]]
        f a xs@(x : xs')
          | succ (card'rank a) == card'rank (head x) = (a : x) : xs'
          | otherwise = [a] : xs

-- | Try to form a wheel from a collection of cards.
--   A wheel is a straight from Ace to Five.
maybeWheel :: [Card] -> Maybe [Card]
maybeWheel cards
  | length filteredCards == 5 = Just filteredCards
  | otherwise = Nothing
  where
    filteredCards :: [Card]
    filteredCards = (flip elem [Ace, Two, Three, Four, Five] . card'rank) `filter` cards

-- | Check for groups of cards in a collection of cards.
checkGroups :: [Card] -> (HandRank, [Card])
checkGroups hand = (handRank, cards)
  where
    groups :: [[Card]]
    groups = sortByLength $ groupBy ((==) `on` card'rank) $ sort hand

    cards :: [Card]
    cards = take 5 $ concat groups

    groupedRankLengths :: [Int]
    groupedRankLengths = length <$> groups

    handRank :: HandRank
    handRank = evalGroupedRanks groupedRankLengths

    evalGroupedRanks :: [RankGroup] -> HandRank
    evalGroupedRanks = \case
      (4 : _) -> FourOfAKind
      (3 : 2 : _) -> FullHouse
      (3 : _) -> ThreeOfAKind
      (2 : 2 : _) -> TwoPair
      (2 : _) -> Pair
      _ -> HighCard

-- | Sort a list of lists by the length of the inner lists.
sortByLength :: Ord a => [[a]] -> [[a]]
sortByLength = sortBy (flip (comparing length) <> flip compare)

-- | Sort a list of 'SeatHand's by the 'HandRank' of the 'RankedHand'.
evaluatePlayerHands :: [SeatHand] -> [SeatHand]
evaluatePlayerHands [] = []
evaluatePlayerHands (x : xs) = foldl f [x] xs
  where
    f :: [SeatHand] -> SeatHand -> [SeatHand]
    f p@(y:ys) z = case playerHand'rankedHand y `compare` playerHand'rankedHand z of
      GT -> p
      EQ -> z : p
      LT -> [z]
