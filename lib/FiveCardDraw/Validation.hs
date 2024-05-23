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

import qualified Data.Map as Map

import FiveCardDraw.Types       (Chips, DrawChoices, GameError(..), GameError(..), Hand, Player(..),
                                 Seat, GameCtx(..), PlayerStatus(..), PostedAnte(..), Deck(..),
                                 InHandStatus(..), BettingAction (..))
import Control.Monad.State      (MonadState(..))
import Control.Monad.Except     (MonadError(..))
import Data.Maybe               (isJust)
import FiveCardDraw.Utils.Utils (freeSeats)
import Data.Monoid (Last(Last))

type Bundle m = (MonadState GameCtx m, MonadError GameError m)

success ::  Bundle m => m (Either GameError ())
success = return $ Right ()

dealCards :: Bundle m => m (Either GameError ())
dealCards = canDealCards
  where
    canDealCards :: Bundle m =>  m (Either GameError ())
    canDealCards = do
      ctx <- get
      let cond1 = deckIsComplete ctx
          cond2 = atLeast2PlayersSatIn ctx
          cond3 = allSatInPlayersHavePostedAnte ctx
          cond4 = hasDesignatedDealer ctx
      case (cond1, cond2, cond3, cond4) of
        (False, _, _, _) -> throwError $ DeckIncomplete "Cannot deal cards, deck has less than 52 cards"
        (_, False, _, _) -> throwError $ NotEnoughSatInPlayers "Cannot deal cards, less than 2 players sat in hand"
        (_, _, False, _) -> throwError $ NotAllSatInPlayersHavePostedAnte "Cannot deal cards, not all sat in players have posted ante"
        (_, _, _, False) -> throwError $ HandHasNoDesignatedDealer "Cannot deal cards, hand has no designated dealer"
        _ -> success

designateDealer :: Bundle m => Seat -> m (Either GameError ())
designateDealer seat = canDesignateDealer
  where
    canDesignateDealer :: Bundle m =>  m (Either GameError ())
    canDesignateDealer = do
      ctx <- get
      let cond1 = playerIsSatIn seat ctx
          cond2 = playerHasPostedAnte seat ctx
          cond3 = not $ hasDesignatedDealer ctx
      case (cond1, cond2, cond3) of
        (False, _, _) -> throwError $ PlayerNotSatIn $ "Cannot designate " <> show seat <> " as dealer, player not sat in"
        (_, False, _) -> throwError $ PlayerHasNotPostedAnte $ "Cannot designate " <> show seat <> " as dealer, player has not posted ante"
        (_, _, False) -> throwError $ HandAlreadyHasDealerDesignated $ "Cannot designate " <> show seat <> " as dealer, hand already has dealer designated"
        _ -> success

takeSeat :: Bundle m => Player -> m (Either GameError ())
takeSeat player = canTakeSeat
  where
    canTakeSeat :: Bundle m =>  m (Either GameError ())
    canTakeSeat = do
      ctx <- get
      let cond1 = not $ playerHasSeat player
          cond2 = freeSeatAvailable ctx
      case (cond1, cond2) of
        (False, _) -> throwError $ PlayerAlreadyHasSeat $ "Cannot take seat, player: " <> show (player'name player) <> " already has a seat: " <> show (player'seat player)
        (_, False) -> throwError $ NoFreeSeatsAvailable $ "Player: " <> show (player'name player) <> " cannot take seat, no free seats available"
        _ -> success

leaveSeat :: Bundle m => Seat -> m (Either GameError ())
leaveSeat seat = canLeaveSeat
  where
    canLeaveSeat :: Bundle m =>  m (Either GameError ())
    canLeaveSeat = do
      ctx <- get
      let cond1 = seatHasPlayer seat ctx
          cond2 = playerIsSatOut seat ctx
      case (cond1, cond2) of
        (False, _) -> throwError $ NoPlayerAtSeat $ "Cannot leave seat, no player at seat: " <> show seat
        (_, False) -> throwError $ PlayerNotSatOut $ "Cannot leave seat, player at seat: " <> show seat <> " not sat out"
        _ -> success

postAnte :: Bundle m => Seat -> m (Either GameError ())
postAnte seat = canPostAnte
  where
    canPostAnte :: Bundle m =>  m (Either GameError ())
    canPostAnte = do
      ctx <- get
      let cond1 = playerIsSatIn seat ctx
          cond2 = not $ playerHasPostedAnte seat ctx
          cond3 = playerHasEnoughChipsForAnte seat ctx
      case (cond1, cond2, cond3) of
        (False, _, _) -> throwError $ PlayerNotSatIn $ "Cannot post ante, player at seat " <> show seat <> " not sat in"
        (_, False, _) -> throwError $ PlayerHasPostedAnte $ "Cannot post ante, player at seat " <> show seat <> " has already posted ante"
        (_, _, False) -> throwError $ InsufficientChips $ "Cannot post ante, player at seat " <> show seat <> " has insufficient chips"
        _ -> success

fold :: Bundle m => Seat -> m (Either GameError ())
fold seat = canFold
  where
    canFold :: Bundle m =>  m (Either GameError ())
    canFold = do
      ctx <- get
      let cond1 = not $ inHandPlayerHasLastBettingAction seat ctx
          cond2 = playerIsInHand seat ctx
          cond3 = playerCanAct seat ctx
      case (cond1, cond2, cond3) of
        (False, _, _) -> throwError $ PlayerHasLastBettingAction $ "Cannot fold, player at seat " <> show seat <> " has already acted this round"
        (_, False, _) -> throwError $ PlayerNotInHand $ "Cannot fold, player at seat " <> show seat <> " not in hand"
        (_, _, False) -> throwError $ PlayerCannotAct  $ "Cannot fold, player at seat " <> show seat <> " cannot act"
        _ -> success

call :: Bundle m => Seat -> m (Either GameError ())
call seat = canCall
  where
    canCall :: Bundle m =>  m (Either GameError ())
    canCall = do
      ctx <- get
      let cond1 = not $ inHandPlayerHasLastBettingAction seat ctx
          cond2 = playerIsInHand seat ctx
          cond3 = playerCanAct seat ctx
          cond4 = betPlaced ctx
          cond5 = playerHasEnoughChipsForCall seat ctx
      case (cond1, cond2, cond3, cond4, cond5) of
        (False, _, _, _, _) -> throwError $ PlayerHasLastBettingAction $ "Cannot call, player at seat " <> show seat <> " has already acted this round"
        (_, False, _, _, _) -> throwError $ PlayerNotInHand $ "Cannot call, player at seat " <> show seat <> " not in hand"
        (_, _, False, _, _) -> throwError $ PlayerCannotAct $ "Cannot call, player at seat " <> show seat <> " cannot act"
        (_, _, _, False, _) -> throwError $ NoBetPlaced $ "Player at seat " <> show seat <> " cannot call, no bet has been placed" 
        (_, _, _, _, False) -> throwError $ InsufficientChips $ "Cannot call, player at seat " <> show seat <> " has insufficient chips to call"
        _ -> success

--       case (cond1, cond2, cond3, cond4) of
--         (False, _, _, _) -> throwError $ PlayerHasLastBettingAction $ "Cannot call, player at seat " <> show seat <> " has already acted this round"
--         (_, False, _, _) -> throwError $ PlayerNotInHand $ "Cannot call, player at seat " <> show seat <> " not in hand"
--         (_, _, False, _) -> throwError $ PlayerCannotAct $ "Cannot call, player at seat " <> show seat <> " cannot act"
--         (_, _, _, False) -> throwError $ InsufficientChips $ "Cannot call, player at seat " <> show seat <> " has insufficient chips to call"
--         _ -> success

raise :: Bundle m => Seat -> Chips -> m (Either GameError ())
raise seat chips = canRaise
  where
    canRaise :: Bundle m =>  m (Either GameError ())
    canRaise = do
      ctx <- get
      let cond1 = not $ inHandPlayerHasLastBettingAction seat ctx
          cond2 = playerIsInHand seat ctx
          cond3 = playerCanAct seat ctx
          cond4 = playerHasEnoughChipsForRaise seat ctx
          cond5 = raiseIsHigherThanCurrentBet seat ctx chips
          cond6 = betPlaced ctx
      case (cond1, cond2, cond3, cond4, cond5, cond6) of
        (False, _, _, _, _, _) -> throwError $ PlayerHasLastBettingAction $ "Cannot raise, player at seat " <> show seat <> " has already acted this round"
        (_, False, _, _, _, _) -> throwError $ PlayerNotInHand $ "Cannot raise, player at seat " <> show seat <> " not in hand"
        (_, _, False, _, _, _) -> throwError $ PlayerCannotAct $ "Cannot raise, player at seat " <> show seat <> " cannot act"
        (_, _, _, False, _, _) -> throwError $ InsufficientChips $ "Cannot raise, player at seat " <> show seat <> " has insufficient chips to raise"
        (_, _, _, _, False, _) -> throwError $ RaiseNotHigherThanCurrentBet $ "Cannot raise, player at seat " <> show seat <> " has not raised higher than current bet"
        (_, _, _, _, _, False) -> throwError $ NoBetPlaced $ "Player at seat " <> show seat <> " cannot raise, no bet has been placed"
        _ -> success

bet :: Bundle m => Seat -> Chips -> m (Either GameError ())
bet seat chips = canBet
  where
    canBet :: Bundle m =>  m (Either GameError ())
    canBet = do
      ctx <- get
      let cond1 = not $ inHandPlayerHasLastBettingAction seat ctx
          cond2 = not $ betPlaced ctx
          cond3 = playerIsInHand seat ctx
          cond4 = playerCanAct seat ctx
          cond5 = playerHasEnoughChipsForBet seat ctx chips
      case (cond1, cond2, cond3, cond4, cond5) of
        (False, _, _, _, _) -> throwError $ PlayerHasLastBettingAction $ "Cannot bet, player at seat " <> show seat <> " has already acted this round"
        (_, False, _, _, _) -> throwError $ BetPlaced "Cannot bet, another player has already placed a bet"
        (_, _, False, _, _) -> throwError $ PlayerNotInHand  $ "Cannot bet, player at seat " <> show seat <> " not in hand"
        (_, _, _, False, _) -> throwError $ PlayerCannotAct $ "Cannot bet, player at seat " <> show seat <> " cannot act"
        (_, _, _, _, False) -> throwError $ InsufficientChips $ "Cannot bet, player at seat " <> show seat <> " has insufficient chips to bet"
        _ -> success

check :: Bundle m => Seat -> m (Either GameError ())
check seat = canCheck
  where
    canCheck :: Bundle m =>  m (Either GameError ())
    canCheck = do
      ctx <- get
      let cond1 = not $ inHandPlayerHasLastBettingAction seat ctx
          cond2 = playerIsInHand seat ctx
          cond3 = playerCanAct seat ctx
          cond4 = not $ betPlaced ctx
      case (cond1, cond2, cond3, cond4) of
        (False, _, _, _) -> throwError $ PlayerHasLastBettingAction $ "Cannot check, player at seat " <> show seat <> " has already acted this round"
        (_, False, _, _) -> throwError $ PlayerNotInHand $ "Cannot check, player at seat " <> show seat <> " not in hand"
        (_, _, False, _) -> throwError $ PlayerCannotAct $ "Cannot check, player at seat " <> show seat <> " cannot act"
        (_, _, _, False) -> throwError $ BetPlaced "Cannot check, another player has already placed a bet"
        _ -> success

draw :: Bundle m => Seat -> DrawChoices -> m (Either GameError ())
draw seat drawSelection = canDraw
  where
    canDraw :: Bundle m =>  m (Either GameError ())
    canDraw = do
      ctx <- get
      let cond1 = cardsDealt ctx
          cond2 = playerIsInHand seat ctx
          cond3 = playerCanAct seat ctx
      case (cond1, cond2, cond3) of
        (False, _, _) -> throwError $ CardsNotDealt $ "Cannot draw, cards not dealt to player at seat " <> show seat
        (_, False, _) -> throwError $ PlayerNotInHand $ "Cannot draw, player at seat " <> show seat <> " not in hand"
        (_, _, False) -> throwError $ PlayerCannotAct $ "Cannot draw, player at seat " <> show seat <> " cannot act"
        _ -> success


muckHand :: Bundle m => Seat -> m (Either GameError ())
muckHand seat = success

sitIn :: Bundle m => Seat -> m (Either GameError ())
sitIn seat = canSitIn
  where
    canSitIn :: Bundle m =>  m (Either GameError ())
    canSitIn = do
      ctx <- get
      if playerIsInHand seat ctx
        then throwError $ PlayerInHand $ "Cannot sit in, player at seat " <> show seat <> " is in hand"
        else success

sitOut :: Bundle m => Seat -> m (Either GameError ())
sitOut seat = canSitOut
  where
    canSitOut :: Bundle m =>  m (Either GameError ())
    canSitOut = do
      ctx <- get
      if playerIsInHand seat ctx
        then throwError $ PlayerInHand $ "Cannot sit out, player at seat " <> show seat <> " is in hand"
        else success

endRound :: Bundle m => m (Either GameError ())
endRound = canEndRound
  where
    canEndRound :: Bundle m =>  m (Either GameError ())
    canEndRound = do
      ctx <- get
      if allInHandPlayersHaveLastBettingAction ctx
        then success
        else throwError $ NotAllPlayersHaveActed "Cannot end round, not all players have acted"

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

deckIsComplete :: GameCtx -> Bool
deckIsComplete GameCtx{..} = length (unDeck gameCtx'deck) == 52

hasDesignatedDealer :: GameCtx -> Bool
hasDesignatedDealer GameCtx{..} = isJust gameCtx'dealer

playerHasPostedAnte :: Seat -> GameCtx -> Bool
playerHasPostedAnte seat GameCtx{..} = maybe False hasPostedAnte player
  where
    player :: Maybe Player
    player = Map.lookup seat gameCtx'players

    hasPostedAnte :: Player -> Bool
    hasPostedAnte Player{player'status} = case player'status of
      SatIn HasPostedAnte -> True
      _ -> False

allSatInPlayersHavePostedAnte :: GameCtx -> Bool
allSatInPlayersHavePostedAnte ctx@GameCtx{..} =
  all (`playerHasPostedAnte` ctx) (seatedPlayers ctx)

playerIsSatIn :: Seat -> GameCtx -> Bool
playerIsSatIn seat GameCtx{..} = maybe False playerStatusIsSatIn player
  where
    player :: Maybe Player
    player = Map.lookup seat gameCtx'players

    playerStatusIsSatIn :: Player -> Bool
    playerStatusIsSatIn Player{..} = case player'status of
      SatIn _ -> True
      _ -> False

playerIsSatOut :: Seat -> GameCtx -> Bool
playerIsSatOut seat = not . playerIsSatIn seat

playerIsInHand :: Seat -> GameCtx -> Bool
playerIsInHand seat GameCtx{..} = maybe False playerStatusIsInHand player
  where
    player :: Maybe Player
    player = Map.lookup seat gameCtx'players

    playerStatusIsInHand :: Player -> Bool
    playerStatusIsInHand Player{..} = case player'status of
      InHand _ -> True
      _ -> False

playerCanAct :: Seat -> GameCtx -> Bool
playerCanAct seat GameCtx{..} = maybe False playerStatusCanAct player
  where
    player :: Maybe Player
    player = Map.lookup seat gameCtx'players

    playerStatusCanAct :: Player -> Bool
    playerStatusCanAct Player{..} = case player'status of
      InHand (CanAct _) -> True
      _ -> False

seatHasPlayer :: Seat -> GameCtx -> Bool
seatHasPlayer seat GameCtx{..} = isJust $ Map.lookup seat gameCtx'players

playerHasEnoughChipsForAnte :: Seat -> GameCtx -> Bool
playerHasEnoughChipsForAnte seat GameCtx{..} = maybe False playerHasEnoughChips player
  where
    player :: Maybe Player
    player = Map.lookup seat gameCtx'players

    playerHasEnoughChips :: Player -> Bool
    playerHasEnoughChips Player{..} = player'chips >= gameCtx'ante

playerHasEnoughChipsForCall :: Seat -> GameCtx -> Bool
playerHasEnoughChipsForCall seat GameCtx{..} = maybe False playerHasEnoughChips player
  where
    player :: Maybe Player
    player = Map.lookup seat gameCtx'players

    playerHasEnoughChips :: Player -> Bool
    playerHasEnoughChips Player{..} = player'chips >= gameCtx'bet

playerHasEnoughChipsForBet :: Seat -> GameCtx -> Chips -> Bool
playerHasEnoughChipsForBet seat GameCtx{..} chips = maybe False playerHasEnoughChips player
  where
    player :: Maybe Player
    player = Map.lookup seat gameCtx'players

    playerHasEnoughChips :: Player -> Bool
    playerHasEnoughChips Player{..} = player'chips >= chips

playerHasEnoughChipsForRaise :: Seat -> GameCtx -> Bool
playerHasEnoughChipsForRaise seat GameCtx{..} = maybe False playerHasEnoughChips player
  where
    player :: Maybe Player
    player = Map.lookup seat gameCtx'players

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

playerHasSeat :: Player -> Bool
playerHasSeat Player{..} = isJust player'seat

freeSeatAvailable :: GameCtx -> Bool
freeSeatAvailable = not . null . freeSeats

allInHandPlayersHaveLastBettingAction :: GameCtx -> Bool
allInHandPlayersHaveLastBettingAction ctx@GameCtx{..} =
  all (`inHandPlayerHasLastBettingAction` ctx) (canActPlayers ctx)

inHandPlayerHasLastBettingAction :: Seat -> GameCtx -> Bool
inHandPlayerHasLastBettingAction seat GameCtx{..} = 
  maybe False playerStatusHasLastBettingAction player
  where
    player :: Maybe Player
    player = Map.lookup seat gameCtx'players

    playerStatusHasLastBettingAction :: Player -> Bool
    playerStatusHasLastBettingAction Player{..} = case player'status 
      of InHand (CanAct (Last (Just _))) -> True
         _ -> False