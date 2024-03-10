module FiveCardDraw.Utils.Utils where

import qualified Data.Map as Map

import FiveCardDraw.Types (player'handL, player'statusL,
                           Card(Card), Chips(Chips),
                           Deck(..), DrawChoice(..),
                           DrawChoices(..), GameCtx(..),
                           Hand(..), InHandStatus(..),
                           Player(..), PlayerStatus(..),
                           Players, PostedAnte(..), Round(..),
                           Seat, SeatHand(..), SplitChips(..), Winners(..)) 
import FiveCardDraw.Hands (valueOfHand)
import System.Random      (Random (randomR), RandomGen, mkStdGen)
import Data.List          ((\\), mapAccumR)
import Data.Maybe         (listToMaybe, fromMaybe, fromJust)
import Data.Bifunctor     (first, Bifunctor (bimap))
import Data.Function      (on)
import Control.Lens       ((&), (.~), (?~))

fisherYatesStep :: forall g a. RandomGen g => (Map.Map Int a, g) -> (Int, a) -> (Map.Map Int a, g)
fisherYatesStep (m, gen) (i, x) =
  ((Map.insert j x . Map.insert i (m Map.! j)) m, gen')
  where
    (j :: Int, gen' :: g) = randomR (0, i) gen

-- shuffle using the Fisher Yates algorithm
shuffle :: RandomGen g => g -> [a] -> ([a], g)
shuffle gen [] = ([], gen)
shuffle gen l = toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems :: (Map.Map k a, b) -> ([a], b)
    toElems (x, y) = (Map.elems x, y)

    numerate :: [a] -> [(Int, a)]
    numerate = zip [1 ..]

    initial :: a -> g -> (Map.Map Int a, g)
    initial x gen = (Map.singleton 0 x, gen)

mkGameFromSeed :: Int -> Int -> GameCtx
mkGameFromSeed ante seed = GameCtx
  { gameCtx'deck = mkShuffledDeck seed
  , gameCtx'pot = Chips 0
  , gameCtx'ante = Chips ante
  , gameCtx'round = PreDrawRound
  , gameCtx'bet = Chips 0
  , gameCtx'dealer = Nothing
  , gameCtx'players = mempty
  , gameCtx'winners = Nothing
  }

mkPlayer :: String -> Int -> Player
mkPlayer playerName chipsAmount = Player
  { player'name = playerName
  , player'hand = Nothing
  , player'chips = Chips chipsAmount
  , player'bet = Chips 0
  , player'committed = Chips 0
  , player'status = SatOut
  , player'seat = Nothing
  }

-- | A standard deck of cards.
initialDeck :: Deck
initialDeck = Deck $ Card <$> [minBound ..] <*> [minBound ..]

mkShuffledDeck :: Int -> Deck
mkShuffledDeck seed = shuffledDeck $ mkStdGen seed

-- Get a shuffled deck of cards.
shuffledDeck :: RandomGen g => g -> Deck
shuffledDeck gen = Deck <$> fst $ shuffle gen (unDeck initialDeck)

takeFreeSeat :: GameCtx -> Maybe Seat
takeFreeSeat = listToMaybe . freeSeats

freeSeats :: GameCtx -> [Seat]
freeSeats GameCtx{..} = [minBound..maxBound] \\ takenSeats
  where
    takenSeats :: [Seat]
    takenSeats = fst <$> Map.toList gameCtx'players

nextRound :: Round -> Round
nextRound PreDrawRound = PostDrawRound
nextRound PostDrawRound = PostDrawRound

rankPlayerHand :: Player -> Maybe SeatHand
rankPlayerHand Player{..} = SeatHand . valueOfHand <$> player'hand <*> player'seat

drawNCards :: Int -> Deck -> ([Card], Deck)
drawNCards n (Deck cs) = (cards, Deck remainingDeck)
  where
    (cards :: [Card] , remainingDeck :: [Card]) = splitAt n cs

dealHand :: Deck -> (Hand, Deck)
dealHand (Deck cs) = (Hand c1 c2 c3 c4 c5, remainingDeck)
  where
    ([c1, c2, c3, c4, c5], remainingDeck) = drawNCards 5 (Deck cs)

dealToPlayers :: Deck -> Players -> (Deck, Players)
dealToPlayers = mapAccumR $ \deck player -> case player'status player of
  SatIn HasPostedAnte -> 
    let (hand, remainingDeck) = dealHand deck
    in (remainingDeck, player 
         & player'handL ?~ hand
         & player'statusL .~ InHand (CanAct mempty))
  _ -> (deck, player)

replaceCardsWithDraw :: Deck -> DrawChoices -> Hand -> (Hand, Deck)
replaceCardsWithDraw deck DrawChoices{..} hand = (hand
  { hand'card1 = fromMaybe (hand'card1 hand) card1
  , hand'card2 = fromMaybe (hand'card2 hand) card2
  , hand'card3 = fromMaybe (hand'card3 hand) card3
  , hand'card4 = fromMaybe (hand'card4 hand) card4
  , hand'card5 = fromMaybe (hand'card5 hand) card5
  }, remainingDeck'''')
  where
    drawCard :: DrawChoice -> Deck -> (Maybe Card, Deck)
    drawCard Discard deck = first listToMaybe $ drawNCards 1 deck
    drawCard Keep deck = (Nothing, deck)
    (card1, remainingDeck) = drawCard draw'choice1 deck
    (card2, remainingDeck') = drawCard draw'choice2 remainingDeck
    (card3, remainingDeck'') = drawCard draw'choice3 remainingDeck'
    (card4, remainingDeck''') = drawCard draw'choice4 remainingDeck''
    (card5, remainingDeck'''') = drawCard draw'choice5 remainingDeck'''

splitChipsAmongWinners :: Chips -> Int -> SplitChips
splitChipsAmongWinners (Chips chips) = uncurry (SplitChips `on` Chips) . divMod chips

winnersLength :: Winners -> Int
winnersLength (SinglePlayerShowdown _) = 1
winnersLength (MultiPlayerShowdown xs) = length xs

winnerSeats :: Winners -> [Seat]
winnerSeats (SinglePlayerShowdown seat) = [seat]
winnerSeats (MultiPlayerShowdown seats) = playerHand'seat <$> seats

isWinner :: Winners -> Seat -> Bool
isWinner winners seat = seat `elem` winnerSeats winners

playerNearestToLeftOfDealer :: GameCtx -> Maybe Seat
playerNearestToLeftOfDealer GameCtx{..} = do
  dealer <- gameCtx'dealer
  let players = Map.toList gameCtx'players
      playerToLeftOfDealer = snd <$> take (succ $ fromEnum dealer) players
  listToMaybe playerToLeftOfDealer >>= player'seat

-- The following functions should only be used once validation has been performed

takeFreeSeatUnsafe :: GameCtx -> Seat
takeFreeSeatUnsafe = fromJust . takeFreeSeat

