module FiveCardDraw.Validation
  ( dealCards
  , designateDealer
  , takeSeat
  , leaveSeat
  , postAnte
  , fold
  , call
  , raise
  , bet
  , check
  , draw
  , muckHand
  , sitIn
  , sitOut
  , endRound
  ) where

import qualified Data.Map                 as Map

import           Control.Lens             ((%~), (&), (+~), (-~), (.~), (?~))
import           Control.Monad            (join)
import           Control.Monad.Except     (MonadError (..))
import           Control.Monad.State      (MonadState (..), modify)
import           Data.Maybe               (isJust, mapMaybe)
import           Data.Monoid              (Last (Last))
import           FiveCardDraw.Types       (BettingAction (..), Chips, Deck (..),
                                           DrawChoices, GameCtx (..),
                                           GameError (..), Hand,
                                           InHandStatus (..), Player (..),
                                           PlayerStatus (..), PostedAnte (..),
                                           Round (..), BettingAction (..), Seat, gameCtx'playersL, player'statusL)
import           FiveCardDraw.Utils.Utils (freeSeats, getPlayerByName)

type Bundle m = (MonadState GameCtx m, MonadError GameError m)

success ::  Bundle m => m (Either GameError ())
success = return $ Right ()

dealCards :: Bundle m => m (Either GameError ())
dealCards = canDealCards
  where
    canDealCards :: Bundle m =>  m (Either GameError ())
    canDealCards = do
      ctx <- get
      if | not $ atLeast2PlayersSatIn ctx          -> throwError NotEnoughSatInPlayers -- "Cannot deal cards, less than 2 players sat in hand"
         | not $ allSatInPlayersHavePostedAnte ctx -> throwError NotAllSatInPlayersHavePostedAnte -- "Cannot deal cards, not all sat in players have posted ante"
         | not $ hasDesignatedDealer ctx           -> throwError HandHasNoDesignatedDealer -- "Cannot deal cards, hand has no designated dealer"
         | otherwise                               -> success

designateDealer :: Bundle m => Seat -> m (Either GameError ())
designateDealer seat = canDesignateDealer
  where
    canDesignateDealer :: Bundle m =>  m (Either GameError ())
    canDesignateDealer = do
      ctx <- get
      if | not $ playerIsSatIn seat ctx       -> throwError PlayerNotSatIn -- "Cannot designate " <> show seat <> " as dealer, player not sat in"
         | not $ playerHasPostedAnte seat ctx -> throwError PlayerHasNotPostedAnte -- "Cannot designate " <> show seat <> " as dealer, player has not posted ante"
         | otherwise                          -> success

takeSeat :: Bundle m => Player -> m (Either GameError ())
takeSeat player = canTakeSeat
  where
    canTakeSeat :: Bundle m =>  m (Either GameError ())
    canTakeSeat = do
      ctx <- get
      if | playerHasSeat player ctx    -> throwError PlayerAlreadyHasSeat -- "Cannot take seat, player: " <> show (player'name player) <> " already has a seat: " <> show (player'seat player)
         | not $ freeSeatAvailable ctx -> throwError NoFreeSeatsAvailable -- "Player: " <> show (player'name player) <> " cannot take seat, no free seats available"
         | otherwise                   -> success

leaveSeat :: Bundle m => Seat -> m (Either GameError ())
leaveSeat seat = canLeaveSeat
  where
    canLeaveSeat :: Bundle m =>  m (Either GameError ())
    canLeaveSeat = do
      ctx <- get
      if | not $ seatHasPlayer seat ctx  -> throwError NoPlayerAtSeat -- "Cannot leave seat, no player at seat: " <> show seat
         | not $ playerIsSatOut seat ctx -> throwError PlayerNotSatOut -- "Cannot leave seat, player at seat: " <> show seat <> " not sat out"
         | otherwise                     -> success

postAnte :: Bundle m => Seat -> m (Either GameError ())
postAnte seat = canPostAnte
  where
    canPostAnte :: Bundle m =>  m (Either GameError ())
    canPostAnte = do
      ctx <- get
      if | not $ playerIsSatIn seat ctx               -> throwError PlayerNotSatIn -- "Cannot post ante, player at seat " <> show seat <> " not sat in"
         | playerHasPostedAnte seat ctx               -> throwError PlayerHasPostedAnte -- "Cannot post ante, player at seat " <> show seat <> " has already posted ante"
         | not $ playerHasEnoughChipsForAnte seat ctx -> throwError InsufficientChips -- "Cannot post ante, player at seat " <> show seat <> " has insufficient chips"
         | otherwise                                  -> success

fold :: Bundle m => Seat -> m (Either GameError ())
fold seat = canFold
  where
    canFold = do
      ctx <- get
      if | inHandPlayerHasLastBettingAction seat ctx -> ifBetRaisedRequiresAction seat canFold ctx
         | not $ playerIsInHand seat ctx             -> throwError PlayerNotInHand -- "Cannot fold, player at seat " <> show seat <> " not in hand"
         | not $ playerCanAct seat ctx               -> throwError PlayerCannotAct  -- "Cannot fold, player at seat " <> show seat <> " cannot act"
         | otherwise                                 -> success

call :: Bundle m => Seat -> m (Either GameError ())
call seat = canCall
  where
    canCall = do
      ctx <- get
      if | inHandPlayerHasLastBettingAction seat ctx  -> ifBetRaisedRequiresAction seat canCall ctx
         | not $ playerIsInHand seat ctx              -> throwError PlayerNotInHand -- "Cannot call, player at seat " <> show seat <> " not in hand"
         | not $ playerCanAct seat ctx                -> throwError PlayerCannotAct -- "Cannot call, player at seat " <> show seat <> " cannot act"
         | not $ playerHasEnoughChipsForCall seat ctx -> throwError InsufficientChips -- "Cannot call, player at seat " <> show seat <> " has insufficient chips to call"
         | not $ betPlaced ctx                        -> throwError NoBetPlaced -- "Player at seat " <> show seat <> " cannot call, no bet has been placed"
         | otherwise                                  -> success

raise :: Bundle m => Seat -> Chips -> m (Either GameError ())
raise seat chips = canRaise
  where
    canRaise = do
      ctx <- get
      if | inHandPlayerHasLastBettingAction seat ctx       -> ifBetRaisedRequiresAction seat canRaise ctx
         | not $ playerIsInHand seat ctx                   -> throwError PlayerNotInHand -- "Cannot raise, player at seat " <> show seat <> " not in hand"
         | not $ playerCanAct seat ctx                     -> throwError PlayerCannotAct -- "Cannot raise, player at seat " <> show seat <> " cannot act"
         | not $ raiseIsHigherThanCurrentBet seat ctx chips-> throwError RaiseNotHigherThanCurrentBet -- "Cannot raise, player at seat " <> show seat <> " has not raised higher than current bet"
         | not $ playerHasEnoughChipsForRaise seat ctx     -> throwError InsufficientChips -- "Cannot raise, player at seat " <> show seat <> " has insufficient chips to raise"
         | not $ betPlaced ctx                             -> throwError NoBetPlaced -- "Player at seat " <> show seat <> " cannot raise, no bet has been placed"
         | otherwise                                       -> success

bet :: Bundle m => Seat -> Chips -> m (Either GameError ())
bet seat chips = canBet
  where
    canBet :: Bundle m =>  m (Either GameError ())
    canBet = do
      ctx <- get
      if | chips < 0                                       -> throwError InvalidBetAmount -- "Cannot bet, invalid bet amount: " <> show chips
         | inHandPlayerHasLastBettingAction seat ctx       -> if playerHasChecked ctx seat
          then modify (setInHandStatusCanAct seat) >> canBet
          else throwError PlayerHasLastBettingAction -- "Cannot bet, player at seat " <> show seat <> " has already acted this round"
         | betPlaced ctx                                   -> throwError BetPlaced -- "Cannot bet, another player has already placed a bet"
         | not $ playerIsInHand seat ctx                   -> throwError PlayerNotInHand  -- "Cannot bet, player at seat " <> show seat <> " not in hand"
         | not $ playerCanAct seat ctx                     -> throwError PlayerCannotAct -- "Cannot bet, player at seat " <> show seat <> " cannot act"
         | not $ playerHasEnoughChipsForBet seat chips ctx -> throwError InsufficientChips -- "Cannot bet, player at seat " <> show seat <> " has insufficient chips to bet"
         | otherwise                                       -> success

check :: Bundle m => Seat -> m (Either GameError ())
check seat = canCheck
  where
    canCheck :: Bundle m =>  m (Either GameError ())
    canCheck = do
      ctx <- get
      if | inHandPlayerHasLastBettingAction seat ctx -> throwError PlayerHasLastBettingAction -- "Cannot check, player at seat " <> show seat <> " has already acted this round"
         | not $ playerIsInHand seat ctx             -> throwError PlayerNotInHand -- "Cannot check, player at seat " <> show seat <> " not in hand"
         | not $ playerCanAct seat ctx               -> throwError PlayerCannotAct -- "Cannot check, player at seat " <> show seat <> " cannot act"
         | betPlaced ctx                             -> throwError BetPlaced -- "Cannot check, another player has already placed a bet"
         | otherwise                                 -> success

draw :: Bundle m => Seat -> DrawChoices -> m (Either GameError ())
draw seat drawSelection = canDraw
  where
    canDraw :: Bundle m =>  m (Either GameError ())
    canDraw = do
      ctx <- get
      if | not $ inDrawRound ctx         -> throwError NotInDrawRound -- "Cannot draw, not in draw round"
         | not $ playerIsInHand seat ctx -> throwError PlayerNotInHand -- "Cannot draw, player at seat " <> show seat <> " not in hand"
         | not $ playerCanAct seat ctx   -> throwError PlayerCannotAct -- "Cannot draw, player at seat " <> show seat <> " cannot act"
         | not $ cardsDealt ctx          -> throwError CardsNotDealt -- "Cannot draw, cards not dealt to player at seat " <> show seat
         | otherwise                     -> success

muckHand :: Bundle m => Seat -> m (Either GameError ())
muckHand seat = canMuckHand
  where
    canMuckHand :: Bundle m =>  m (Either GameError ())
    canMuckHand = do
      ctx <- get
      if playerHasFolded seat ctx
        then success
        else throwError PlayerCanStillAct -- "Cannot muck hand, player at seat " <> show seat <> " has not folded"

sitIn :: Bundle m => Seat -> m (Either GameError ())
sitIn seat = canSitIn
  where
    canSitIn :: Bundle m =>  m (Either GameError ())
    canSitIn = do
      GameCtx{gameCtx'round} <- get
      if gameCtx'round /= SetupRound
        then throwError CanOnlyPerformOperationInSetupRound -- $ "Cannot sit player in hand, round is not setup round"
        else success

sitOut :: Bundle m => Seat -> m (Either GameError ())
sitOut seat = canSitOut
  where
    canSitOut :: Bundle m =>  m (Either GameError ())
    canSitOut = do
      GameCtx{gameCtx'round} <- get
      if gameCtx'round /= SetupRound
        then throwError CanOnlyPerformOperationInSetupRound -- $ "Cannot sit player in hand, round is not setup round"
        else success

endRound :: Bundle m => m (Either GameError ())
endRound = canEndRound
  where
    canEndRound :: Bundle m =>  m (Either GameError ())
    canEndRound = do
      ctx@GameCtx{gameCtx'round} <- get
      case gameCtx'round of
        SetupRound -> if cardsDealt ctx
          then success
          else throwError CardsNotDealt
        _ -> if | not $ allInHandPlayersHaveLastBettingAction ctx -> throwError NotAllPlayersHaveActed
                | betPlacedWithCheckedPlayers ctx -> throwError BetPlacedWithCheckedPlayers
                | playerHasBetLowerThanRoundBet ctx -> throwError PlayerHasBetLowerThanRoundBet
                | otherwise -> success

betPlacedWithCheckedPlayers :: GameCtx -> Bool
betPlacedWithCheckedPlayers ctx = betPlaced ctx && any (playerHasChecked ctx) (inHandPlayers ctx)

playerHasChecked :: GameCtx -> Seat -> Bool
playerHasChecked ctx seat = maybe False playerStatusChecked $ lookupPlayerBySeat seat ctx
  where
    playerStatusChecked :: Player -> Bool
    playerStatusChecked Player{..} =
      player'status == InHand (CanAct (Last (Just Checked)))

playerHasBetLowerThanRoundBet :: GameCtx -> Bool
playerHasBetLowerThanRoundBet ctx@GameCtx{gameCtx'bet} = any (< gameCtx'bet) (playerBets (inHandPlayers ctx) ctx)

playerBets :: [Seat] -> GameCtx -> [Chips]
playerBets seats ctx = mapMaybe (getBet ctx) seats
  where
    getBet :: GameCtx -> Seat -> Maybe Chips
    getBet ctx seat = player'bet <$> lookupPlayerBySeat seat ctx

occupiedSeats :: GameCtx -> [Seat]
occupiedSeats GameCtx{..} = Map.keys gameCtx'players

seatedPlayers :: GameCtx -> [Seat]
seatedPlayers ctx = filter (`playerIsSatIn` ctx) (occupiedSeats ctx)

inHandPlayers :: GameCtx -> [Seat]
inHandPlayers ctx = filter (`playerIsInHand` ctx) (occupiedSeats ctx)

canActPlayers :: GameCtx -> [Seat]
canActPlayers ctx = filter (`playerCanAct` ctx) (inHandPlayers ctx)

atLeast2PlayersSatIn :: GameCtx -> Bool
atLeast2PlayersSatIn ctx = length (seatedPlayers ctx) >= 2

hasDesignatedDealer :: GameCtx -> Bool
hasDesignatedDealer GameCtx{..} = isJust gameCtx'dealer

playerHasPostedAnte :: Seat -> GameCtx -> Bool
playerHasPostedAnte seat = maybe False hasPostedAnte . lookupPlayerBySeat seat
  where
    hasPostedAnte :: Player -> Bool
    hasPostedAnte Player{player'status} = player'status == SatIn HasPostedAnte

allSatInPlayersHavePostedAnte :: GameCtx -> Bool
allSatInPlayersHavePostedAnte ctx@GameCtx{..} =
  all (`playerHasPostedAnte` ctx) (seatedPlayers ctx)

playerIsSatIn :: Seat -> GameCtx -> Bool
playerIsSatIn seat = maybe False playerStatusIsSatIn . lookupPlayerBySeat seat
  where
    playerStatusIsSatIn :: Player -> Bool
    playerStatusIsSatIn Player{..} = case player'status of
      SatIn _ -> True
      _       -> False

playerIsSatOut :: Seat -> GameCtx -> Bool
playerIsSatOut seat = not . playerIsSatIn seat

inDrawRound :: GameCtx -> Bool
inDrawRound GameCtx{gameCtx'round} =
  gameCtx'round == DrawRound1 || gameCtx'round == DrawRound2

playerIsInHand :: Seat -> GameCtx -> Bool
playerIsInHand seat = maybe False playerStatusIsInHand . lookupPlayerBySeat seat
  where
    playerStatusIsInHand :: Player -> Bool
    playerStatusIsInHand Player{..} = case player'status of
      InHand _ -> True
      _        -> False

playerHasFolded :: Seat -> GameCtx -> Bool
playerHasFolded seat = maybe False playerStatusIsInHandFolded . lookupPlayerBySeat seat
  where
    playerStatusIsInHandFolded :: Player -> Bool
    playerStatusIsInHandFolded Player{..} = case player'status of
      InHand Folded -> True
      _             -> False

playerCanAct :: Seat -> GameCtx -> Bool
playerCanAct seat = maybe False playerStatusCanAct . lookupPlayerBySeat seat
  where
    playerStatusCanAct :: Player -> Bool
    playerStatusCanAct Player{..} = case player'status of
      InHand (CanAct _) -> True
      _                 -> False

seatHasPlayer :: Seat -> GameCtx -> Bool
seatHasPlayer seat = isJust . lookupPlayerBySeat seat

playerHasEnoughChipsForAnte :: Seat -> GameCtx -> Bool
playerHasEnoughChipsForAnte seat ctx@GameCtx{..} = maybe False playerHasEnoughChips $ lookupPlayerBySeat seat ctx
  where
    playerHasEnoughChips :: Player -> Bool
    playerHasEnoughChips Player{..} = player'chips >= gameCtx'ante

playerHasEnoughChipsForCall :: Seat -> GameCtx -> Bool
playerHasEnoughChipsForCall seat ctx@GameCtx{..} = maybe False playerHasEnoughChips $ lookupPlayerBySeat seat ctx
  where
    playerHasEnoughChips :: Player -> Bool
    playerHasEnoughChips Player{..} = player'chips >= gameCtx'bet

playerHasEnoughChipsForBet :: Seat -> Chips -> GameCtx -> Bool
playerHasEnoughChipsForBet seat chips = maybe False playerHasEnoughChips . lookupPlayerBySeat seat
  where
    playerHasEnoughChips :: Player -> Bool
    playerHasEnoughChips Player{..} = player'chips >= chips

playerHasEnoughChipsForRaise :: Seat -> GameCtx -> Bool
playerHasEnoughChipsForRaise seat ctx@GameCtx{..} = maybe False playerHasEnoughChips $  lookupPlayerBySeat seat ctx
  where
    playerHasEnoughChips :: Player -> Bool
    playerHasEnoughChips Player{..} = player'chips > gameCtx'bet

raiseIsHigherThanCurrentBet :: Seat -> GameCtx -> Chips -> Bool
raiseIsHigherThanCurrentBet seat GameCtx{..} chips = chips > gameCtx'bet

betPlaced :: GameCtx -> Bool
betPlaced GameCtx{..} = gameCtx'bet > 0

cardsDealt :: GameCtx -> Bool
cardsDealt GameCtx{..} =
  Map.size (Map.filter playerHasHand gameCtx'players) == Map.size gameCtx'players
  where
    playerHasHand :: Player -> Bool
    playerHasHand Player{..} = isJust player'hand

playerHasSeat :: Player -> GameCtx -> Bool
playerHasSeat Player{player'name} ctx = isJust $ player'seat =<< getPlayerByName player'name ctx

freeSeatAvailable :: GameCtx -> Bool
freeSeatAvailable = not . null . freeSeats

allInHandPlayersHaveLastBettingAction :: GameCtx -> Bool
allInHandPlayersHaveLastBettingAction ctx@GameCtx{..} =
  all (`inHandPlayerHasLastBettingAction` ctx) (canActPlayers ctx)

inHandPlayerHasLastBettingAction :: Seat -> GameCtx -> Bool
inHandPlayerHasLastBettingAction seat ctx@GameCtx{..} =
  maybe False playerStatusHasLastBettingAction $  lookupPlayerBySeat seat ctx
  where
    playerStatusHasLastBettingAction :: Player -> Bool
    playerStatusHasLastBettingAction Player{..} = case player'status
      of InHand (CanAct (Last (Just _))) -> True
         _                               -> False

lookupPlayerBySeat :: Seat -> GameCtx -> Maybe Player
lookupPlayerBySeat seat GameCtx{..} = Map.lookup seat gameCtx'players


ifBetRaisedRequiresAction :: Bundle m => Seat -> m b -> GameCtx -> m b
ifBetRaisedRequiresAction seat action ctx
  | playerHasChecked ctx seat = modify (setInHandStatusCanAct seat) >> action
  | playerHasBetLowerThanRoundBet ctx = modify (setInHandStatusCanAct seat) >> action
  | otherwise = throwError PlayerHasLastBettingAction

setInHandStatusCanAct :: Seat -> GameCtx -> GameCtx
setInHandStatusCanAct seat ctx = ctx & gameCtx'playersL %~ Map.adjust setPlayerInHandStatusCanAct seat
  where
    setPlayerInHandStatusCanAct :: Player -> Player
    setPlayerInHandStatusCanAct player = player & player'statusL .~ InHand (CanAct (Last Nothing))