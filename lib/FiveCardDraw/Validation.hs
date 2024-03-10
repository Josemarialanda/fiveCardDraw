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
        (False, _, _, _) -> throwError DeckIncomplete
        (_, False, _, _) -> throwError NotEnoughSatInPlayers
        (_, _, False, _) -> throwError NotAllSatInPlayersHavePostedAnte
        (_, _, _, False) -> throwError HandHasNoDesignatedDealer
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
        (False, _, _) -> throwError PlayerNotSatIn
        (_, False, _) -> throwError PlayerHasNotPostedAnte
        (_, _, False) -> throwError HandAlreadyHasDealerDesignated
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
        (False, _) -> throwError PlayerAlreadyHasSeat
        (_, False) -> throwError NoFreeSeatsAvailable
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
        (False, _) -> throwError NoPlayerAtSeat
        (_, False) -> throwError PlayerNotSatOut
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
        (False, _, _) -> throwError PlayerNotSatIn
        (_, False, _) -> throwError PlayerHasPostedAnte
        (_, _, False) -> throwError InsufficientChips
        _ -> success

fold :: Bundle m => Seat -> m (Either GameError ())
fold seat = canFold
  where
    canFold :: Bundle m =>  m (Either GameError ())
    canFold = do
      ctx <- get
      let cond1 = not $ playerHasLastBettingAction seat ctx
          cond2 = playerIsInHand seat ctx
          cond3 = playerCanAct seat ctx
      case (cond1, cond2, cond3) of
        (False, _, _) -> throwError PlayerHasLastBettingAction
        (_, False, _) -> throwError PlayerNotInHand
        (_, _, False) -> throwError PlayerCannotAct
        _ -> success

call :: Bundle m => Seat -> m (Either GameError ())
call seat = canCall
  where
    canCall :: Bundle m =>  m (Either GameError ())
    canCall = do
      ctx <- get
      let cond1 = not $ playerHasLastBettingAction seat ctx
          cond2 = playerIsInHand seat ctx
          cond3 = playerCanAct seat ctx
          cond4 = playerHasEnoughChipsForCall seat ctx
      case (cond1, cond2, cond3, cond4) of
        (False, _, _, _) -> throwError PlayerHasLastBettingAction
        (_, False, _, _) -> throwError PlayerNotInHand
        (_, _, False, _) -> throwError PlayerCannotAct
        (_, _, _, False) -> throwError InsufficientChips
        _ -> success

raise :: Bundle m => Seat -> Chips -> m (Either GameError ())
raise seat chips = canRaise
  where
    canRaise :: Bundle m =>  m (Either GameError ())
    canRaise = do
      ctx <- get
      let cond1 = not $ playerHasLastBettingAction seat ctx
          cond2 = playerIsInHand seat ctx
          cond3 = playerCanAct seat ctx
          cond4 = playerHasEnoughChipsForCall seat ctx
          cond5 = playerHasEnoughChipsForBet seat ctx chips
      case (cond1, cond2, cond3, cond4, cond5) of
        (False, _, _, _, _) -> throwError PlayerHasLastBettingAction
        (_, False, _, _, _) -> throwError PlayerNotInHand
        (_, _, False, _, _) -> throwError PlayerCannotAct
        (_, _, _, False, _) -> throwError InsufficientChips
        (_, _, _, _, False) -> throwError InsufficientChips
        _ -> success

bet :: Bundle m => Seat -> Chips -> m (Either GameError ())
bet seat chips = canBet
  where
    canBet :: Bundle m =>  m (Either GameError ())
    canBet = do
      ctx <- get
      let cond1 = not $ playerHasLastBettingAction seat ctx
          cond2 = not $ betPlaced ctx
          cond3 = playerIsInHand seat ctx
          cond4 = playerCanAct seat ctx
          cond5 = playerHasEnoughChipsForBet seat ctx chips
      case (cond1, cond2, cond3, cond4, cond5) of
        (False, _, _, _, _) -> throwError PlayerHasLastBettingAction
        (_, False, _, _, _) -> throwError BetAlreadyPlaced
        (_, _, False, _, _) -> throwError PlayerNotInHand
        (_, _, _, False, _) -> throwError PlayerCannotAct
        (_, _, _, _, False) -> throwError InsufficientChips
        _ -> success

check :: Bundle m => Seat -> m (Either GameError ())
check seat = canCheck
  where
    canCheck :: Bundle m =>  m (Either GameError ())
    canCheck = do
      ctx <- get
      let cond1 = not $ playerHasLastBettingAction seat ctx
          cond2 = playerIsInHand seat ctx
          cond3 = playerCanAct seat ctx
          cond4 = not $ betPlaced ctx
      case (cond1, cond2, cond3, cond4) of
        (False, _, _, _) -> throwError PlayerHasLastBettingAction
        (_, False, _, _) -> throwError PlayerNotInHand
        (_, _, False, _) -> throwError PlayerCannotAct
        (_, _, _, False) -> throwError BetAlreadyPlaced
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
        (False, _, _) -> throwError CardsNotDealt
        (_, False, _) -> throwError PlayerNotInHand
        (_, _, False) -> throwError PlayerCannotAct
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
        then throwError PlayerInHand
        else success

sitOut :: Bundle m => Seat -> m (Either GameError ())
sitOut seat = canSitOut
  where
    canSitOut :: Bundle m =>  m (Either GameError ())
    canSitOut = do
      ctx <- get
      if playerIsInHand seat ctx
        then throwError PlayerInHand
        else success

endRound :: Bundle m => m (Either GameError ())
endRound = canEndRound
  where
    canEndRound :: Bundle m =>  m (Either GameError ())
    canEndRound = do
      ctx <- get
      if allPlayersHaveLastBettingAction ctx
        then success
        else throwError NotAllPlayersHaveActed

occupiedSeats :: GameCtx -> [Seat]
occupiedSeats GameCtx{..} = Map.keys gameCtx'players

seatedPlayers :: GameCtx -> [Seat]
seatedPlayers ctx = filter (flip playerIsSatIn ctx) (occupiedSeats ctx)

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
  all (flip playerHasPostedAnte ctx) (seatedPlayers ctx)

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

allPlayersHaveLastBettingAction :: GameCtx -> Bool
allPlayersHaveLastBettingAction _ = True

playerHasLastBettingAction :: Seat -> GameCtx -> Bool
playerHasLastBettingAction _ _ = False