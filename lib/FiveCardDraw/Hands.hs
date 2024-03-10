module FiveCardDraw.Hands where

import FiveCardDraw.Types (Card(..), Hand(..), HandRank(..), SeatHand(..), 
                           RankedHand(..), Rank(..))
import Data.List          (foldl', groupBy, nubBy, sort, sortBy)
import Data.Ord           (comparing)
import Data.Function      (on)

type RankGroup = Int

value :: [Card] -> (HandRank, [Card])
value hand = maybe (ifNotFlush hand) ifFlush (maybeFlush hand)

handToCards :: Hand -> [Card]
handToCards Hand{..} = [hand'card1, hand'card2, hand'card3, hand'card4, hand'card5]

valueOfHand :: Hand -> RankedHand
valueOfHand hand = RankedHand{rankedHand'handRank = handRank, rankedHand'hand = hand}
  where
    (handRank, _) = value $ handToCards hand

ifNotFlush :: [Card] -> (HandRank, [Card])
ifNotFlush hand = maybe (checkGroups hand) (Straight,) (maybeStraight hand)

ifFlush :: [Card] -> (HandRank, [Card])
ifFlush hand =
  maybe (Flush, take 5 hand) (StraightFlush,) (maybeStraight hand)

lastNelems :: Int -> [a] -> [a]
lastNelems n xs = foldl' (const . drop 1) xs (drop n xs)

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

maybeStraight :: [Card] -> Maybe [Card]
maybeStraight cards
  | length cs'' >= 5 = Just (lastNelems 5 cs'')
  | otherwise = maybeWheel cardsUniqRanks
  where
    cardsUniqRanks :: [Card]
    cardsUniqRanks = nubBy ((==) `on` card'rank) cards

    cs'' :: [Card]
    cs'' = head $ sortByLength $ groupBySuccCards $ sort cardsUniqRanks

maybeWheel :: [Card] -> Maybe [Card]
maybeWheel cards
  | length filteredCards == 5 = Just filteredCards
  | otherwise = Nothing
  where
    filteredCards :: [Card]
    filteredCards = (flip elem [Ace, Two, Three, Four, Five] . card'rank) `filter` cards

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

groupBySuccCards :: [Card] -> [[Card]]
groupBySuccCards = foldr f []
  where
    f :: Card -> [[Card]] -> [[Card]]
    f a [] = [[a]]
    f a xs@(x : xs')
      | succ (card'rank a) == card'rank (head x) = (a : x) : xs'
      | otherwise = [a] : xs

sortByLength :: Ord a => [[a]] -> [[a]]
sortByLength = sortBy (flip (comparing length) <> flip compare)

evaluatePlayerHands :: [SeatHand] -> [SeatHand]
evaluatePlayerHands [] = []
evaluatePlayerHands (x : xs) = foldl f [x] xs
  where
    f :: [SeatHand] -> SeatHand -> [SeatHand]
    f p@(y:ys) z = case playerHand'rankedHand y `compare` playerHand'rankedHand z of
      GT -> p
      EQ -> z : p
      LT -> [z]