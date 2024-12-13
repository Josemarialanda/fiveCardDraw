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
import           FiveCardDraw.Types       (Chips, DrawChoices, GameCtx (..),
                                           GameError (..), Player, Round (..),
                                           Seat, Bundle)
import           FiveCardDraw.Utils.Utils (allInHandPlayersHaveLastBettingAction,
                                           allSatInPlayersHavePostedAnte,
                                           atLeast2PlayersSatIn, betPlaced,
                                           betPlacedWithCheckedPlayers,
                                           cardsDealt, freeSeatAvailable,
                                           hasDesignatedDealer,
                                           ifBetRaisedRequiresAction,
                                           inDrawRound,
                                           inHandPlayerHasLastBettingAction,
                                           playerCanAct,
                                           playerHasBetLowerThanRoundBet,
                                           playerHasChecked,
                                           playerHasEnoughChipsForAnte,
                                           playerHasFolded, playerHasPostedAnte,
                                           playerHasSeat, playerIsInHand,
                                           playerIsSatIn, playerIsSatOut,
                                           raiseIsHigherThanCurrentBet,
                                           seatHasPlayer, setInHandStatusCanAct)

success ::  Bundle m => m (Either GameError ())
success = return $ Right ()

dealCards :: forall m. Bundle m => m (Either GameError ())
dealCards = get >>= canDealCards
  where
    canDealCards :: GameCtx -> m (Either GameError ())
    canDealCards ctx
      | not $ atLeast2PlayersSatIn ctx          = throwError NotEnoughSatInPlayers            -- ^ Cannot deal cards, less than 2 players sat in hand
      | not $ allSatInPlayersHavePostedAnte ctx = throwError NotAllSatInPlayersHavePostedAnte -- ^ Cannot deal cards, not all sat in players have posted ante
      | not $ hasDesignatedDealer ctx           = throwError HandHasNoDesignatedDealer        -- ^ Cannot deal cards, hand has no designated dealer
      | otherwise                               = success

designateDealer :: forall m. Bundle m => Seat -> m (Either GameError ())
designateDealer seat = get >>= canDesignateDealer
  where
    canDesignateDealer :: GameCtx -> m (Either GameError ())
    canDesignateDealer ctx
      | not $ playerIsSatIn seat ctx       = throwError PlayerNotSatIn         -- ^ Cannot designate player as dealer that is not sat in
      | not $ playerHasPostedAnte seat ctx = throwError PlayerHasNotPostedAnte -- ^ Cannot designate player that has not posted ante as dealer
      | otherwise                          = success

takeSeat :: forall m. Bundle m => Player -> m (Either GameError ())
takeSeat player = get >>= canTakeSeat
  where
    canTakeSeat :: GameCtx -> m (Either GameError ())
    canTakeSeat ctx
      | playerHasSeat player ctx    = throwError PlayerAlreadyHasSeat -- ^ Player cannot take seat if they already have a seat
      | not $ freeSeatAvailable ctx = throwError NoFreeSeatsAvailable -- ^ Player cannot take seat if there are no free seats available
      | otherwise                   = success

leaveSeat :: forall m. Bundle m => Seat -> m (Either GameError ())
leaveSeat seat = get >>= canLeaveSeat
  where
    canLeaveSeat :: GameCtx -> m (Either GameError ())
    canLeaveSeat ctx
      | not $ seatHasPlayer seat ctx  = throwError NoPlayerAtSeat  -- ^ Player cannot leave seat if there is no player at seat
      | not $ playerIsSatOut seat ctx = throwError PlayerNotSatOut -- ^ Player cannot leave seat if they are not sat out
      | otherwise                     = success

postAnte :: forall m. Bundle m => Seat -> m (Either GameError ())
postAnte seat = get >>= canPostAnte
  where
    canPostAnte :: GameCtx -> m (Either GameError ())
    canPostAnte ctx
      | not $ playerIsSatIn seat ctx               = throwError PlayerNotSatIn      -- ^ Player cannot post ante if they are not sat in
      | playerHasPostedAnte seat ctx               = throwError PlayerHasPostedAnte -- ^ Player cannot post ante twice
      | not $ playerHasEnoughChipsForAnte seat ctx = throwError InsufficientChips   -- ^ Player cannot post ante if they do not have enough chips
      | otherwise                                  = success

fold :: forall m. Bundle m => Seat -> m (Either GameError ())
fold seat = canFold
  where
    canFold :: m (Either GameError ())
    canFold = do
      ctx <- get
      if | inHandPlayerHasLastBettingAction seat ctx -> ifBetRaisedRequiresAction seat canFold ctx -- ^ InHand player with last betting action can fold if bet has been raised
         | not $ playerIsInHand seat ctx             -> throwError PlayerNotInHand                 -- ^ Player cannot fold if they are not in hand
         | not $ playerCanAct seat ctx               -> throwError PlayerCannotAct                 -- ^ Player cannot fold if they cannot act
         | otherwise                                 -> success

call :: forall m. Bundle m => Seat -> m (Either GameError ())
call seat = canCall
  where
    canCall :: m (Either GameError ())
    canCall = do
      ctx <- get
      if | inHandPlayerHasLastBettingAction seat ctx  -> ifBetRaisedRequiresAction seat canCall ctx -- ^ InHand player with last betting action can call if bet has been raised
         | not $ playerIsInHand seat ctx              -> throwError PlayerNotInHand                 -- ^ Player cannot call if they are not in hand
         | not $ playerCanAct seat ctx                -> throwError PlayerCannotAct                 -- ^ Player cannot call if they cannot act
         | not $ betPlaced ctx                        -> throwError NoBetPlaced                     -- ^ Player cannot call if no bet has been placed
         | otherwise                                  -> success

raise :: forall m. Bundle m => Seat -> Chips -> m (Either GameError ())
raise seat chips = canRaise
  where
    canRaise :: m (Either GameError ())
    canRaise = do
      ctx <- get
      if | inHandPlayerHasLastBettingAction seat ctx        -> ifBetRaisedRequiresAction seat canRaise ctx -- ^ InHand player with last betting action can raise if bet has been raised
         | not $ playerIsInHand seat ctx                    -> throwError PlayerNotInHand                  -- ^ Player cannot raise if they are not in hand
         | not $ playerCanAct seat ctx                      -> throwError PlayerCannotAct                  -- ^ Player cannot raise if they cannot act
         | not $ raiseIsHigherThanCurrentBet seat ctx chips -> throwError RaiseNotHigherThanCurrentBet     -- ^ Player cannot raise if the placed bet is not higher than the current bet
         | not $ betPlaced ctx                              -> throwError NoBetPlaced                      -- ^ Player cannot raise if no bet has been placed
         | otherwise                                        -> success

bet :: forall m. Bundle m => Seat -> Chips -> m (Either GameError ())
bet seat chips = canBet
  where
    canBet :: m (Either GameError ())
    canBet = do
      ctx <- get
      if | chips < 0                                       -> throwError InvalidBetAmount  -- ^ Bet must be greater than zero
         | inHandPlayerHasLastBettingAction seat ctx       -> if playerHasChecked ctx seat
             then modify (setInHandStatusCanAct seat) >> canBet                            -- ^ InHand player with last betting action can bet if they have checked status
             else throwError PlayerHasLastBettingAction                                    -- ^ InHand player with last betting action cannot bet
         | betPlaced ctx                                   -> throwError BetPlaced         -- ^ Player cannot bet if another player has already placed a bet
         | not $ playerIsInHand seat ctx                   -> throwError PlayerNotInHand   -- ^ Player cannot bet if they are not in hand
         | not $ playerCanAct seat ctx                     -> throwError PlayerCannotAct   -- ^ Player cannot bet if they cannot act
         | otherwise                                       -> success

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

muckHand :: forall m. Bundle m => Seat -> m (Either GameError ())
muckHand seat = get >>= canMuckHand
  where
    canMuckHand :: GameCtx -> m (Either GameError ())
    canMuckHand ctx
      | not $ playerHasFolded seat ctx = throwError PlayerCanStillAct -- ^ Player cannot muck hand if they have not folded
      | otherwise                      = success

sitIn :: forall m. Bundle m => Seat -> m (Either GameError ())
sitIn seat = get >>= canSitIn
  where
    canSitIn :: GameCtx -> m (Either GameError ())
    canSitIn ctx
      | gameCtx'round ctx /= SetupRound = throwError CanOnlyPerformOperationInSetupRound -- ^ "Cannot sit player in hand, round is not setup round"
      | otherwise                       = success

sitOut :: forall m. Bundle m => Seat -> m (Either GameError ())
sitOut seat = get >>= canSitOut
  where
    canSitOut :: GameCtx -> m (Either GameError ())
    canSitOut ctx
      | gameCtx'round ctx /= SetupRound = throwError CanOnlyPerformOperationInSetupRound -- ^ "Cannot sit player in hand, round is not setup round"
      | otherwise                       = success

endRound :: forall m. Bundle m => m (Either GameError ())
endRound = get >>= canEndRound
  where
    canEndRound :: GameCtx -> m (Either GameError ())
    canEndRound ctx = case gameCtx'round ctx of
      SetupRound -> if cardsDealt ctx
        then success
        else throwError CardsNotDealt
      _ -> if | not $ allInHandPlayersHaveLastBettingAction ctx -> throwError NotAllPlayersHaveActed        -- ^ Cannot end round if not all in hand players have acted
              | betPlacedWithCheckedPlayers ctx                 -> throwError BetPlacedWithCheckedPlayers   -- ^ Cannot end round if bet has been placed with checked players
              | playerHasBetLowerThanRoundBet ctx               -> throwError PlayerHasBetLowerThanRoundBet -- ^ Cannot end round if player has bet lower than round bet
              | otherwise                                       -> success
