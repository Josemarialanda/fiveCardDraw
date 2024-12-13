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

import           Control.Monad.Except     (MonadError (..))
import           Control.Monad.State      (MonadState (..), modify)
import           FiveCardDraw.Types       (Bundle, Chips, DrawChoices,
                                           GameCtx (..), GameError (..), Player (..),
                                           Round (..), Seat)
import           FiveCardDraw.Utils.Utils (allInHandPlayersHaveLastBettingAction,
                                           allSatInPlayersHavePostedAnte,
                                           atLeast2PlayersSatIn, betPlaced,
                                           betPlacedWithCheckedPlayers,
                                           cardsDealt, freeSeatAvailable,
                                           hasDesignatedDealer, inDrawRound,
                                           inHandPlayerHasLastBettingAction,
                                           playerCanAct,
                                           playerHasBetLowerThanRoundBet,
                                           playerHasChecked,
                                           playerHasEnoughChipsForAnte,
                                           playerHasFolded, playerHasPostedAnte,
                                           playerIsInHand,
                                           playerIsSatIn, playerIsSatOut,
                                           raiseIsHigherThanCurrentBet,
                                           seatHasPlayer, setInHandStatusCanAct,
                                           tryResetBetAction)
import Data.Maybe (isJust)
import qualified Data.Map as Map

-- | Right () is a success, Left GameError is a failure
success ::  Bundle m => m (Either GameError ())
success = return $ Right ()

-- | Validate that a `DealCards` action can be performed in the current game context
dealCards :: forall m. Bundle m => m (Either GameError ())
dealCards = get >>= canDealCards
  where
    canDealCards :: GameCtx -> m (Either GameError ())
    canDealCards ctx
      | not $ atLeast2PlayersSatIn ctx          = throwError NotEnoughSatInPlayers            -- ^ Cannot deal cards, less than 2 players sat in hand
      | not $ allSatInPlayersHavePostedAnte ctx = throwError NotAllSatInPlayersHavePostedAnte -- ^ Cannot deal cards, not all sat in players have posted ante
      | not $ hasDesignatedDealer ctx           = throwError HandHasNoDesignatedDealer        -- ^ Cannot deal cards, hand has no designated dealer
      | otherwise                               = success

-- | Validate that a `DesignateDealer` action can be performed in the current game context
designateDealer :: forall m. Bundle m => Seat -> m (Either GameError ())
designateDealer seat = get >>= canDesignateDealer
  where
    canDesignateDealer :: GameCtx -> m (Either GameError ())
    canDesignateDealer ctx
      | not $ playerIsSatIn seat ctx       = throwError PlayerNotSatIn         -- ^ Cannot designate player as dealer that is not sat in
      | not $ playerHasPostedAnte seat ctx = throwError PlayerHasNotPostedAnte -- ^ Cannot designate player that has not posted ante as dealer
      | otherwise                          = success

-- | Validate that a `TakeSeat` action can be performed in the current game context
takeSeat :: forall m. Bundle m => Player -> m (Either GameError ())
takeSeat player = get >>= canTakeSeat
  where
    canTakeSeat :: GameCtx -> m (Either GameError ())
    canTakeSeat ctx
      | playerHasSeat player ctx    = throwError PlayerAlreadyHasSeat -- ^ Player cannot take seat if they already have a seat
      | not $ freeSeatAvailable ctx = throwError NoFreeSeatsAvailable -- ^ Player cannot take seat if there are no free seats available
      | otherwise                   = success

-- | Validate that a `LeaveSeat` action can be performed in the current game context
leaveSeat :: forall m. Bundle m => Seat -> m (Either GameError ())
leaveSeat seat = get >>= canLeaveSeat
  where
    canLeaveSeat :: GameCtx -> m (Either GameError ())
    canLeaveSeat ctx
      | not $ seatHasPlayer seat ctx  = throwError NoPlayerAtSeat  -- ^ Player cannot leave seat if there is no player at seat
      | not $ playerIsSatOut seat ctx = throwError PlayerNotSatOut -- ^ Player cannot leave seat if they are not sat out
      | otherwise                     = success

-- | Validate that a `PostAnte` action can be performed in the current game context
postAnte :: forall m. Bundle m => Seat -> m (Either GameError ())
postAnte seat = get >>= canPostAnte
  where
    canPostAnte :: GameCtx -> m (Either GameError ())
    canPostAnte ctx
      | not $ playerIsSatIn seat ctx               = throwError PlayerNotSatIn      -- ^ Player cannot post ante if they are not sat in
      | playerHasPostedAnte seat ctx               = throwError PlayerHasPostedAnte -- ^ Player cannot post ante twice
      | not $ playerHasEnoughChipsForAnte seat ctx = throwError InsufficientChips   -- ^ Player cannot post ante if they do not have enough chips
      | otherwise                                  = success

-- | Validate that a `Fold` action can be performed in the current game context
fold :: forall m. Bundle m => Seat -> m (Either GameError ())
fold seat = get >>= canFold
  where
    canFold :: GameCtx -> m (Either GameError ())
    canFold ctx
      | inHandPlayerHasLastBettingAction seat ctx = tryResetBetAction seat (fold seat) ctx -- ^ InHand player with last betting action can fold if bet has been raised
      | not $ playerIsInHand seat ctx             = throwError PlayerNotInHand             -- ^ Player cannot fold if they are not in hand
      | not $ playerCanAct seat ctx               = throwError PlayerCannotAct             -- ^ Player cannot fold if they cannot act
      | otherwise                                 = success

-- | Validate that a `Call` action can be performed in the current game context
call :: forall m. Bundle m => Seat -> m (Either GameError ())
call seat = get >>= canCall
  where
    canCall :: GameCtx -> m (Either GameError ())
    canCall ctx
      | inHandPlayerHasLastBettingAction seat ctx = tryResetBetAction seat (call seat) ctx -- ^ InHand player with last betting action can call if bet has been raised
      | not $ playerIsInHand seat ctx             = throwError PlayerNotInHand             -- ^ Player cannot call if they are not in hand
      | not $ playerCanAct seat ctx               = throwError PlayerCannotAct             -- ^ Player cannot call if they cannot act
      | not $ betPlaced ctx                       = throwError NoBetPlaced                 -- ^ Player cannot call if no bet has been placed
      | otherwise                                 = success

-- | Validate that a `Raise` action can be performed in the current game context
raise :: forall m. Bundle m => Seat -> Chips -> m (Either GameError ())
raise seat chips = get >>= canRaise
  where
    canRaise :: GameCtx -> m (Either GameError ())
    canRaise ctx
      | inHandPlayerHasLastBettingAction seat ctx        = tryResetBetAction seat (raise seat chips) ctx -- ^ InHand player with last betting action can raise if bet has been raised
      | not $ playerIsInHand seat ctx                    = throwError PlayerNotInHand                    -- ^ Player cannot raise if they are not in hand
      | not $ playerCanAct seat ctx                      = throwError PlayerCannotAct                    -- ^ Player cannot raise if they cannot act
      | not $ raiseIsHigherThanCurrentBet seat ctx chips = throwError RaiseNotHigherThanCurrentBet       -- ^ Player cannot raise if the placed bet is not higher than the current bet
      | not $ betPlaced ctx                              = throwError NoBetPlaced                        -- ^ Player cannot raise if no bet has been placed
      | otherwise                                        = success

-- | Validate that a `Bet` action can be performed in the current game context
bet :: forall m. Bundle m => Seat -> Chips -> m (Either GameError ())
bet seat chips = get >>= canBet
  where
    canBet :: GameCtx -> m (Either GameError ())
    canBet ctx
      | chips < 0                                       = throwError InvalidBetAmount           -- ^ Bet must be greater than zero
      | inHandPlayerHasLastBettingAction seat ctx       = throwError PlayerHasLastBettingAction -- ^ InHand player with last betting action cannot bet
      | betPlaced ctx                                   = throwError BetPlaced                  -- ^ Player cannot bet if another player has already placed a bet
      | not $ playerIsInHand seat ctx                   = throwError PlayerNotInHand            -- ^ Player cannot bet if they are not in hand
      | not $ playerCanAct seat ctx                     = throwError PlayerCannotAct            -- ^ Player cannot bet if they cannot act
      | otherwise                                       = success

-- | Validate that a `Check` action can be performed in the current game context
check :: forall m. Bundle m => Seat -> m (Either GameError ())
check seat = get >>= canCheck
  where
    canCheck :: GameCtx -> m (Either GameError ())
    canCheck ctx
      | inHandPlayerHasLastBettingAction seat ctx = throwError PlayerHasLastBettingAction -- ^ InHand player with last betting action cannot check
      | not $ playerIsInHand seat ctx             = throwError PlayerNotInHand            -- ^ Player cannot check if they are not in hand
      | not $ playerCanAct seat ctx               = throwError PlayerCannotAct            -- ^ Player cannot check if they cannot act
      | betPlaced ctx                             = throwError BetPlaced                  -- ^ Player cannot check if another player has already placed a bet
      | otherwise                                 = success

-- | Validate that a `Draw` action can be performed in the current game context
draw :: forall m. Bundle m => Seat -> DrawChoices -> m (Either GameError ())
draw seat drawSelection = get >>= canDraw
  where
    canDraw :: GameCtx -> m (Either GameError ())
    canDraw ctx
      | not $ inDrawRound ctx         = throwError NotInDrawRound  -- ^ Player cannot draw if they are not in draw round
      | not $ playerIsInHand seat ctx = throwError PlayerNotInHand -- ^ Player cannot draw if they are not in hand
      | not $ playerCanAct seat ctx   = throwError PlayerCannotAct -- ^ Player cannot draw if they cannot act
      | not $ cardsDealt ctx          = throwError CardsNotDealt   -- ^ Player cannot draw if cards have not been dealt to them
      | otherwise                     = success

-- | Validate that a `MuckHand` action can be performed in the current game context
muckHand :: forall m. Bundle m => Seat -> m (Either GameError ())
muckHand seat = get >>= canMuckHand
  where
    canMuckHand :: GameCtx -> m (Either GameError ())
    canMuckHand ctx
      | not $ playerHasFolded seat ctx = throwError PlayerCanStillAct -- ^ Player cannot muck hand if they have not folded
      | otherwise                      = success

-- | Validate that a `SitIn` action can be performed in the current game context
sitIn :: forall m. Bundle m => Seat -> m (Either GameError ())
sitIn seat = get >>= canSitIn
  where
    canSitIn :: GameCtx -> m (Either GameError ())
    canSitIn GameCtx{gameCtx'round}
      | gameCtx'round /= SetupRound = throwError CanOnlyPerformOperationInSetupRound -- ^ "Cannot sit player in hand, round is not setup round"
      | otherwise                   = success

-- | Validate that a `SitOut` action can be performed in the current game context
sitOut :: forall m. Bundle m => Seat -> m (Either GameError ())
sitOut seat = get >>= canSitOut
  where
    canSitOut :: GameCtx -> m (Either GameError ())
    canSitOut GameCtx{gameCtx'round}
      | gameCtx'round /= SetupRound = throwError CanOnlyPerformOperationInSetupRound -- ^ "Cannot sit player in hand, round is not setup round"
      | otherwise                   = success

-- | Validate that a `EndRound` action can be performed in the current game context
endRound :: forall m. Bundle m => m (Either GameError ())
endRound = get >>= canEndRound
  where
    canEndRound :: GameCtx -> m (Either GameError ())
    canEndRound ctx@GameCtx{gameCtx'round}
      | gameCtx'round == SetupRound && not (cardsDealt ctx)                            = throwError CardsNotDealt -- ^ Cannot end round if cards have not been dealt
      | gameCtx'round /= SetupRound && not (allInHandPlayersHaveLastBettingAction ctx) = throwError NotAllPlayersHaveActed        -- ^ Cannot end round if not all in hand players have acted
      | gameCtx'round /= SetupRound && betPlacedWithCheckedPlayers ctx                 = throwError BetPlacedWithCheckedPlayers   -- ^ Cannot end round if bet has been placed with checked players
      | gameCtx'round /= SetupRound && playerHasBetLowerThanRoundBet ctx               = throwError PlayerHasBetLowerThanRoundBet -- ^ Cannot end round if player has bet lower than round bet
      | otherwise                                                                      = success

-- Helper functions

-- | Check if a player has a seat in the game context
playerHasSeat :: Player -> GameCtx -> Bool
playerHasSeat player ctx = isJust $ player'seat =<< getPlayerByName (player'name player) ctx
  where
    getPlayerByName :: String -> GameCtx -> Maybe Player
    getPlayerByName playerName GameCtx{..} =
      case Map.elems $ Map.filter ((== playerName) . player'name) gameCtx'players of
        []         -> Nothing
        (player:_) -> Just player