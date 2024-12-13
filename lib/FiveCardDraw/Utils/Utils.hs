module FiveCardDraw.Utils.Utils where

import qualified Data.Map             as Map

import           Control.Lens         ((%~), (&), (+~), (-~), (.~), (?~))
import           Control.Monad.Except (MonadError (..))
import           Control.Monad.State  (modify)
import           Data.Bifunctor       (Bifunctor (bimap), first)
import           Data.Function        (on)
import           Data.List            (mapAccumR, (\\))
import           Data.Maybe           (fromJust, fromMaybe, isJust, listToMaybe,
                                       mapMaybe)
import           Data.Monoid          (Last (Last))
import           FiveCardDraw.Hands   (valueOfHand)
import           FiveCardDraw.Types   (BettingAction (..), Bundle, Card (Card),
                                       Chips (Chips), Deck (..),
                                       DrawChoice (..), DrawChoices (..),
                                       GameCtx (..), GameError (..), Hand (..),
                                       InHandStatus (..), Player (..),
                                       PlayerStatus (..), Players,
                                       PostedAnte (..), Round (..), Seat,
                                       SeatHand (..), SplitChips (..),
                                       Winners (..), gameCtx'playersL,
                                       player'handL, player'statusL)
import           System.Random        (Random (randomR), RandomGen, mkStdGen)

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







mkPlayer :: String -> Int -> FiveCardDraw.Types.Player
mkPlayer playerName chipsAmount = FiveCardDraw.Types.Player
  { player'name = playerName
  , player'hand = Nothing
  , player'chips = FiveCardDraw.Types.Chips chipsAmount
  , player'bet = FiveCardDraw.Types.Chips 0
  , player'committed = FiveCardDraw.Types.Chips 0
  , player'status = FiveCardDraw.Types.SatOut
  , player'seat = Nothing
  }





lookupPlayerBySeat :: FiveCardDraw.Types.Seat -> FiveCardDraw.Types.GameCtx -> Maybe FiveCardDraw.Types.Player
lookupPlayerBySeat seat FiveCardDraw.Types.GameCtx{..} = Map.lookup seat gameCtx'players







takeFreeSeat :: FiveCardDraw.Types.GameCtx -> Maybe FiveCardDraw.Types.Seat
takeFreeSeat = listToMaybe . freeSeats

freeSeats :: FiveCardDraw.Types.GameCtx -> [FiveCardDraw.Types.Seat]
freeSeats FiveCardDraw.Types.GameCtx{..} = [minBound..maxBound] \\ takenSeats
  where
    takenSeats :: [FiveCardDraw.Types.Seat]
    takenSeats = fst <$> Map.toList gameCtx'players

nextRound :: FiveCardDraw.Types.Round -> FiveCardDraw.Types.Round
nextRound FiveCardDraw.Types.SetupRound   = FiveCardDraw.Types.PreDrawRound
nextRound FiveCardDraw.Types.PreDrawRound = FiveCardDraw.Types.DrawRound1
nextRound FiveCardDraw.Types.DrawRound1   = FiveCardDraw.Types.DrawRound2
nextRound FiveCardDraw.Types.DrawRound2   = FiveCardDraw.Types.ShowdownRound

rankPlayerHand :: FiveCardDraw.Types.Player -> Maybe FiveCardDraw.Types.SeatHand
rankPlayerHand FiveCardDraw.Types.Player{..} = FiveCardDraw.Types.SeatHand . valueOfHand <$> player'hand <*> player'seat

drawNCards :: Int -> FiveCardDraw.Types.Deck -> ([FiveCardDraw.Types.Card], FiveCardDraw.Types.Deck)
drawNCards n (FiveCardDraw.Types.Deck cs) = (cards, FiveCardDraw.Types.Deck remainingDeck)
  where
    (cards :: [FiveCardDraw.Types.Card] , remainingDeck :: [FiveCardDraw.Types.Card]) = splitAt n cs

dealHand :: FiveCardDraw.Types.Deck -> (FiveCardDraw.Types.Hand, FiveCardDraw.Types.Deck)
dealHand (FiveCardDraw.Types.Deck cs) = (FiveCardDraw.Types.Hand c1 c2 c3 c4 c5, remainingDeck)
  where
    ([c1, c2, c3, c4, c5], remainingDeck) = drawNCards 5 (FiveCardDraw.Types.Deck cs)

dealToPlayers :: FiveCardDraw.Types.Deck -> FiveCardDraw.Types.Players -> (FiveCardDraw.Types.Deck, FiveCardDraw.Types.Players)
dealToPlayers = mapAccumR $ \deck player -> case player'status player of
  FiveCardDraw.Types.SatIn FiveCardDraw.Types.HasPostedAnte ->
    let (hand, remainingDeck) = dealHand deck
    in (remainingDeck, player
         & FiveCardDraw.Types.player'handL ?~ hand
         & FiveCardDraw.Types.player'statusL .~ FiveCardDraw.Types.InHand (FiveCardDraw.Types.CanAct mempty))
  _ -> (deck, player)

replaceCardsWithDraw :: FiveCardDraw.Types.Deck -> FiveCardDraw.Types.DrawChoices -> FiveCardDraw.Types.Hand -> (FiveCardDraw.Types.Hand, FiveCardDraw.Types.Deck)
replaceCardsWithDraw deck FiveCardDraw.Types.DrawChoices{..} hand = (hand
  { hand'card1 = fromMaybe (hand'card1 hand) card1
  , hand'card2 = fromMaybe (hand'card2 hand) card2
  , hand'card3 = fromMaybe (hand'card3 hand) card3
  , hand'card4 = fromMaybe (hand'card4 hand) card4
  , hand'card5 = fromMaybe (hand'card5 hand) card5
  }, remainingDeck'''')
  where
    drawCard :: FiveCardDraw.Types.DrawChoice -> FiveCardDraw.Types.Deck -> (Maybe FiveCardDraw.Types.Card, FiveCardDraw.Types.Deck)
    drawCard FiveCardDraw.Types.Discard deck = first listToMaybe $ drawNCards 1 deck
    drawCard FiveCardDraw.Types.Keep deck    = (Nothing, deck)
    (card1, remainingDeck) = drawCard draw'choice1 deck
    (card2, remainingDeck') = drawCard draw'choice2 remainingDeck
    (card3, remainingDeck'') = drawCard draw'choice3 remainingDeck'
    (card4, remainingDeck''') = drawCard draw'choice4 remainingDeck''
    (card5, remainingDeck'''') = drawCard draw'choice5 remainingDeck'''

splitChipsAmongWinners :: FiveCardDraw.Types.Chips -> Int -> FiveCardDraw.Types.SplitChips
splitChipsAmongWinners (FiveCardDraw.Types.Chips chips) = uncurry (FiveCardDraw.Types.SplitChips `on` FiveCardDraw.Types.Chips) . divMod chips

winnersLength :: FiveCardDraw.Types.Winners -> Int
winnersLength (FiveCardDraw.Types.SinglePlayerShowdown _) = 1
winnersLength (FiveCardDraw.Types.MultiPlayerShowdown xs) = length xs

winnerSeats :: FiveCardDraw.Types.Winners -> [FiveCardDraw.Types.Seat]
winnerSeats (FiveCardDraw.Types.SinglePlayerShowdown seat) = [seat]
winnerSeats (FiveCardDraw.Types.MultiPlayerShowdown seats) = playerHand'seat <$> seats

isWinner :: FiveCardDraw.Types.Winners -> FiveCardDraw.Types.Seat -> Bool
isWinner winners seat = seat `elem` winnerSeats winners

playerNearestToLeftOfDealer :: FiveCardDraw.Types.GameCtx -> Maybe FiveCardDraw.Types.Seat
playerNearestToLeftOfDealer FiveCardDraw.Types.GameCtx{..} = do
  dealer <- gameCtx'dealer
  let players = Map.toList gameCtx'players
      playerToLeftOfDealer = snd <$> take (succ $ fromEnum dealer) players
  listToMaybe playerToLeftOfDealer >>= player'seat

betPlacedWithCheckedPlayers :: FiveCardDraw.Types.GameCtx -> Bool
betPlacedWithCheckedPlayers ctx = betPlaced ctx && any (playerHasChecked ctx) (inHandPlayers ctx)

playerHasChecked :: FiveCardDraw.Types.GameCtx -> FiveCardDraw.Types.Seat -> Bool
playerHasChecked ctx seat = maybe False playerStatusChecked $ lookupPlayerBySeat seat ctx
  where
    playerStatusChecked :: FiveCardDraw.Types.Player -> Bool
    playerStatusChecked FiveCardDraw.Types.Player{..} =
      player'status == FiveCardDraw.Types.InHand (FiveCardDraw.Types.CanAct (Last (Just Checked)))

playerHasBetLowerThanRoundBet :: FiveCardDraw.Types.GameCtx -> Bool
playerHasBetLowerThanRoundBet ctx@FiveCardDraw.Types.GameCtx{gameCtx'bet} = any (< gameCtx'bet) (playerBets (inHandNotFoldedPlayers ctx) ctx)

playerBets :: [FiveCardDraw.Types.Seat] -> FiveCardDraw.Types.GameCtx -> [FiveCardDraw.Types.Chips]
playerBets seats ctx = mapMaybe (getBet ctx) seats
  where
    getBet :: FiveCardDraw.Types.GameCtx -> FiveCardDraw.Types.Seat -> Maybe FiveCardDraw.Types.Chips
    getBet ctx seat = player'bet <$> lookupPlayerBySeat seat ctx

occupiedSeats :: FiveCardDraw.Types.GameCtx -> [FiveCardDraw.Types.Seat]
occupiedSeats FiveCardDraw.Types.GameCtx{..} = Map.keys gameCtx'players

seatedPlayers :: FiveCardDraw.Types.GameCtx -> [FiveCardDraw.Types.Seat]
seatedPlayers ctx = filter (`playerIsSatIn` ctx) (occupiedSeats ctx)

inHandPlayers :: FiveCardDraw.Types.GameCtx -> [FiveCardDraw.Types.Seat]
inHandPlayers ctx = filter (`playerIsInHand` ctx) (occupiedSeats ctx)

inHandNotFoldedPlayers :: FiveCardDraw.Types.GameCtx -> [FiveCardDraw.Types.Seat]
inHandNotFoldedPlayers ctx = filter (\seat -> not $ playerHasFolded seat ctx) (inHandPlayers ctx)

canActPlayers :: FiveCardDraw.Types.GameCtx -> [FiveCardDraw.Types.Seat]
canActPlayers ctx = filter (`playerCanAct` ctx) (inHandPlayers ctx)

atLeast2PlayersSatIn :: FiveCardDraw.Types.GameCtx -> Bool
atLeast2PlayersSatIn ctx = length (seatedPlayers ctx) >= 2

hasDesignatedDealer :: FiveCardDraw.Types.GameCtx -> Bool
hasDesignatedDealer FiveCardDraw.Types.GameCtx{..} = isJust gameCtx'dealer

playerHasPostedAnte :: FiveCardDraw.Types.Seat -> FiveCardDraw.Types.GameCtx -> Bool
playerHasPostedAnte seat = maybe False hasPostedAnte . lookupPlayerBySeat seat
  where
    hasPostedAnte :: FiveCardDraw.Types.Player -> Bool
    hasPostedAnte FiveCardDraw.Types.Player{player'status} = player'status == FiveCardDraw.Types.SatIn FiveCardDraw.Types.HasPostedAnte

allSatInPlayersHavePostedAnte :: FiveCardDraw.Types.GameCtx -> Bool
allSatInPlayersHavePostedAnte ctx@FiveCardDraw.Types.GameCtx{..} =
  all (`playerHasPostedAnte` ctx) (seatedPlayers ctx)

playerIsSatIn :: FiveCardDraw.Types.Seat -> FiveCardDraw.Types.GameCtx -> Bool
playerIsSatIn seat = maybe False playerStatusIsSatIn . lookupPlayerBySeat seat
  where
    playerStatusIsSatIn :: FiveCardDraw.Types.Player -> Bool
    playerStatusIsSatIn FiveCardDraw.Types.Player{..} = case player'status of
      FiveCardDraw.Types.SatIn _ -> True
      _                          -> False

playerIsSatOut :: FiveCardDraw.Types.Seat -> FiveCardDraw.Types.GameCtx -> Bool
playerIsSatOut seat = not . playerIsSatIn seat

inDrawRound :: FiveCardDraw.Types.GameCtx -> Bool
inDrawRound FiveCardDraw.Types.GameCtx{gameCtx'round} =
  gameCtx'round == FiveCardDraw.Types.DrawRound1 || gameCtx'round == FiveCardDraw.Types.DrawRound2

playerIsInHand :: FiveCardDraw.Types.Seat -> FiveCardDraw.Types.GameCtx -> Bool
playerIsInHand seat = maybe False playerStatusIsInHand . lookupPlayerBySeat seat
  where
    playerStatusIsInHand :: FiveCardDraw.Types.Player -> Bool
    playerStatusIsInHand FiveCardDraw.Types.Player{..} = case player'status of
      FiveCardDraw.Types.InHand _ -> True
      _                           -> False

playerHasFolded :: FiveCardDraw.Types.Seat -> FiveCardDraw.Types.GameCtx -> Bool
playerHasFolded seat = maybe False playerStatusIsInHandFolded . lookupPlayerBySeat seat
  where
    playerStatusIsInHandFolded :: FiveCardDraw.Types.Player -> Bool
    playerStatusIsInHandFolded FiveCardDraw.Types.Player{..} = case player'status of
      FiveCardDraw.Types.InHand FiveCardDraw.Types.Folded -> True
      _                                                   -> False

playerCanAct :: FiveCardDraw.Types.Seat -> FiveCardDraw.Types.GameCtx -> Bool
playerCanAct seat = maybe False playerStatusCanAct . lookupPlayerBySeat seat
  where
    playerStatusCanAct :: FiveCardDraw.Types.Player -> Bool
    playerStatusCanAct FiveCardDraw.Types.Player{..} = case player'status of
      FiveCardDraw.Types.InHand (FiveCardDraw.Types.CanAct _) -> True
      _                                                       -> False

seatHasPlayer :: FiveCardDraw.Types.Seat -> FiveCardDraw.Types.GameCtx -> Bool
seatHasPlayer seat = isJust . lookupPlayerBySeat seat

playerHasEnoughChipsForAnte :: FiveCardDraw.Types.Seat -> FiveCardDraw.Types.GameCtx -> Bool
playerHasEnoughChipsForAnte seat ctx@FiveCardDraw.Types.GameCtx{..} = maybe False playerHasEnoughChips $ lookupPlayerBySeat seat ctx
  where
    playerHasEnoughChips :: FiveCardDraw.Types.Player -> Bool
    playerHasEnoughChips FiveCardDraw.Types.Player{..} = player'chips >= gameCtx'ante

playerHasEnoughChipsForCall :: FiveCardDraw.Types.Seat -> FiveCardDraw.Types.GameCtx -> Bool
playerHasEnoughChipsForCall seat ctx@FiveCardDraw.Types.GameCtx{..} = maybe False playerHasEnoughChips $ lookupPlayerBySeat seat ctx
  where
    playerHasEnoughChips :: FiveCardDraw.Types.Player -> Bool
    playerHasEnoughChips FiveCardDraw.Types.Player{..} = player'chips >= gameCtx'bet

playerHasEnoughChipsForBet :: FiveCardDraw.Types.Seat -> FiveCardDraw.Types.Chips -> FiveCardDraw.Types.GameCtx -> Bool
playerHasEnoughChipsForBet seat chips = maybe False playerHasEnoughChips . lookupPlayerBySeat seat
  where
    playerHasEnoughChips :: FiveCardDraw.Types.Player -> Bool
    playerHasEnoughChips FiveCardDraw.Types.Player{..} = player'chips >= chips

playerHasEnoughChipsForRaise :: FiveCardDraw.Types.Seat -> FiveCardDraw.Types.GameCtx -> Bool
playerHasEnoughChipsForRaise seat ctx@FiveCardDraw.Types.GameCtx{..} = maybe False playerHasEnoughChips $  lookupPlayerBySeat seat ctx
  where
    playerHasEnoughChips :: FiveCardDraw.Types.Player -> Bool
    playerHasEnoughChips FiveCardDraw.Types.Player{..} = player'chips > gameCtx'bet

raiseIsHigherThanCurrentBet :: FiveCardDraw.Types.Seat -> FiveCardDraw.Types.GameCtx -> FiveCardDraw.Types.Chips -> Bool
raiseIsHigherThanCurrentBet seat FiveCardDraw.Types.GameCtx{..} chips = chips > gameCtx'bet

betPlaced :: FiveCardDraw.Types.GameCtx -> Bool
betPlaced FiveCardDraw.Types.GameCtx{..} = gameCtx'bet > 0

cardsDealt :: FiveCardDraw.Types.GameCtx -> Bool
cardsDealt FiveCardDraw.Types.GameCtx{..} =
  Map.size (Map.filter playerHasHand gameCtx'players) == Map.size gameCtx'players
  where
    playerHasHand :: FiveCardDraw.Types.Player -> Bool
    playerHasHand FiveCardDraw.Types.Player{..} = isJust player'hand

freeSeatAvailable :: FiveCardDraw.Types.GameCtx -> Bool
freeSeatAvailable = not . null . freeSeats

allInHandPlayersHaveLastBettingAction :: FiveCardDraw.Types.GameCtx -> Bool
allInHandPlayersHaveLastBettingAction ctx@FiveCardDraw.Types.GameCtx{..} =
  all (`inHandPlayerHasLastBettingAction` ctx) (canActPlayers ctx)

inHandPlayerHasLastBettingAction :: FiveCardDraw.Types.Seat -> FiveCardDraw.Types.GameCtx -> Bool
inHandPlayerHasLastBettingAction seat ctx@FiveCardDraw.Types.GameCtx{..} =
  maybe False playerStatusHasLastBettingAction $  lookupPlayerBySeat seat ctx
  where
    playerStatusHasLastBettingAction :: FiveCardDraw.Types.Player -> Bool
    playerStatusHasLastBettingAction FiveCardDraw.Types.Player{..} = case player'status
      of FiveCardDraw.Types.InHand (FiveCardDraw.Types.CanAct (Last (Just _))) -> True
         _                               -> False

tryResetBetAction :: Bundle m => FiveCardDraw.Types.Seat -> m b -> FiveCardDraw.Types.GameCtx -> m b
tryResetBetAction seat action ctx
  | playerHasChecked ctx seat = modify (setInHandStatusCanAct seat) >> action
  | playerHasBetLowerThanRoundBet ctx = modify (setInHandStatusCanAct seat) >> action
  | otherwise = throwError PlayerHasLastBettingAction

setInHandStatusCanAct :: FiveCardDraw.Types.Seat -> FiveCardDraw.Types.GameCtx -> FiveCardDraw.Types.GameCtx
setInHandStatusCanAct seat ctx = ctx & gameCtx'playersL %~ Map.adjust setPlayerInHandStatusCanAct seat
  where
    setPlayerInHandStatusCanAct :: FiveCardDraw.Types.Player -> FiveCardDraw.Types.Player
    setPlayerInHandStatusCanAct player = player & FiveCardDraw.Types.player'statusL .~ FiveCardDraw.Types.InHand (FiveCardDraw.Types.CanAct (Last Nothing))


-- The following functions should only be used once validation has been performed

takeFreeSeatUnsafe :: FiveCardDraw.Types.GameCtx -> FiveCardDraw.Types.Seat
takeFreeSeatUnsafe = fromJust . takeFreeSeat

isAllIn :: Chips -> Chips -> InHandStatus -> InHandStatus
isAllIn playerChips bet status
  | playerChips <= bet = AllIn
  | otherwise = status

allButOnePlayerFolded :: GameCtx -> Bool
allButOnePlayerFolded ctx@GameCtx{gameCtx'players} = length inHandPlayerSeats - length inHandFoldedPlayerSeats == 1
  where
    playerSeats = Map.keys gameCtx'players
    inHandPlayerSeats = filter (flip playerIsInHand ctx) playerSeats
    inHandFoldedPlayerSeats = filter (flip playerIsInHand ctx) inHandPlayerSeats
