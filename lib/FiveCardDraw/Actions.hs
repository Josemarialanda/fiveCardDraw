module FiveCardDraw.Actions
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

import qualified FiveCardDraw.Validation as Validation
import qualified Data.Map.Strict         as Map

import FiveCardDraw.Types
                                (gameCtx'anteL, gameCtx'betL, gameCtx'dealerL,
                                 gameCtx'deckL, gameCtx'playersL, gameCtx'potL,
                                 gameCtx'roundL, gameCtx'winnersL, player'betL, 
                                 player'chipsL, player'committedL, player'handL,
                                 player'seatL, player'statusL,
                                 BettingAction(..), Chips(Chips), Deck, DrawChoices,
                                 GameCtx(..), GameError, Hand, HasBet(..), InHandStatus(..),
                                 Player(..), PlayerStatus(..), Players, PostedAnte(..),
                                 Round(..), Seat, SeatHand(..), SplitChips(..), Winners(..) )
import FiveCardDraw.Utils.Utils (drawNCards, replaceCardsWithDraw, takeFreeSeatUnsafe, 
                                 rankPlayerHand, splitChipsAmongWinners, winnersLength, 
                                 winnerSeats, isWinner, playerNearestToLeftOfDealer, 
                                 dealHand, dealToPlayers, nextRound)
import Control.Monad.State       (MonadState(..), gets, modify)
import Control.Monad.Except      (MonadError(..))
import Data.List                 (mapAccumR)
import Data.Bifunctor            (first)
import FiveCardDraw.Hands        (evaluatePlayerHands)
import Data.Tuple                (swap)
import Control.Lens              ((&), (%~), (+~), (-~), (.~), (?~))

type Bundle m = (MonadState GameCtx m, MonadError GameError m)

validate :: Bundle m => m b -> m (Either GameError ()) -> m b
validate action validator = validator >>= either throwError (const action)

dealCards :: forall m b. Bundle m => m b -> m b
dealCards next = validate action validator
  where
    validator :: m (Either GameError ())
    validator = Validation.dealCards

    action :: m b
    action = modify dealCards' >> next

designateDealer :: forall m b. Bundle m => Seat -> m b -> m b
designateDealer seat next = validate action validator
  where
    validator :: m (Either GameError ())
    validator = Validation.designateDealer seat

    action :: m b
    action = modify (designateDealer' seat) >> next

takeSeat :: forall m b. Bundle m => Player -> (Seat -> m b) -> m b
takeSeat player next = validate action validator
  where
    validator :: m (Either GameError ())
    validator = Validation.takeSeat player

    action :: m b
    action = do
      ctx <- get
      let (ctx', seat) = takeSeat' player ctx
      put ctx'
      next seat

leaveSeat :: forall m b. Bundle m => Seat -> m b -> m b
leaveSeat seat next = validate action validator
  where
    validator :: m (Either GameError ())
    validator = Validation.leaveSeat seat

    action :: m b
    action = modify (leaveSeat' seat) >> next

postAnte :: forall m b. Bundle m => Seat -> m b -> m b
postAnte seat next = validate action validator
  where
    validator :: m (Either GameError ())
    validator = Validation.postAnte seat

    action :: m b
    action = do
      modify (postAnte' seat) >> next

fold :: forall m b. Bundle m => Seat -> m b -> m b
fold seat next = validate action validator
  where
    validator :: m (Either GameError ())
    validator = Validation.fold seat
    action = modify (fold' seat) >> next

call :: forall m b. Bundle m => Seat -> m b -> m b
call seat next = validate action validator
  where
    validator :: m (Either GameError ())
    validator = Validation.call seat

    action :: m b
    action = do
      currentBet <- gets gameCtx'bet
      modify (bet' seat currentBet) >> next

raise :: forall m b. Bundle m => Seat -> Chips -> m b -> m b
raise seat chips next = validate action validator
  where
    validator :: m (Either GameError ())
    validator = Validation.raise seat chips

    action :: m b
    action = modify (bet' seat chips) >> next

bet :: forall m b. Bundle m => Seat -> Chips -> m b -> m b
bet seat chips next = validate action validator
  where
    validator :: m (Either GameError ())
    validator = Validation.bet seat chips

    action :: m b
    action = modify (bet' seat chips) >> next

check :: forall m b. Bundle m => Seat -> m b -> m b
check seat next = validate action validator
  where
    validator :: m (Either GameError ())
    validator = Validation.check seat

    action :: m b
    action = modify (check' seat) >> next

draw :: forall m b. Bundle m => Seat -> DrawChoices -> m b -> m b
draw seat drawSelection next = validate action validator
  where
    validator :: m (Either GameError ())
    validator = Validation.draw seat drawSelection

    action :: m b
    action = modify (draw' seat drawSelection) >> next

muckHand :: forall m b. Bundle m => Seat -> m b -> m b
muckHand seat next = validate action validator
  where
    validator :: m (Either GameError ())
    validator = Validation.muckHand seat

    action :: m b
    action = modify (muckHand' seat) >> next

sitIn :: forall m b. Bundle m => Seat -> m b -> m b
sitIn seat next = validate action validator
  where
    validator :: m (Either GameError ())
    validator = Validation.sitIn seat

    action :: m b
    action = modify (sitIn' seat) >> next

sitOut :: forall m b. Bundle m => Seat -> m b -> m b
sitOut seat next = validate action validator
  where
    validator :: m (Either GameError ())
    validator = Validation.sitOut seat

    action :: m b
    action = modify (sitOut' seat) >> next

endRound :: forall m b. Bundle m => m b -> m b
endRound next = validate action validator
  where
    validator :: m (Either GameError ())
    validator = Validation.endRound

    action :: m b
    action = modify endRound' >> next

dealCards' :: GameCtx -> GameCtx
dealCards' ctx@GameCtx{..} = ctx
  & gameCtx'playersL .~ dealtPlayers
  & gameCtx'deckL .~ remainingDeck
  where
    remainingDeck :: Deck
    dealtPlayers :: Players
    (remainingDeck, dealtPlayers) = dealToPlayers gameCtx'deck gameCtx'players

designateDealer' :: Seat -> GameCtx -> GameCtx
designateDealer' seat = gameCtx'dealerL ?~ seat

takeSeat' :: Player -> GameCtx -> (GameCtx, Seat)
takeSeat' player ctx = (gameCtx', freeSeat)
  where
    freeSeat :: Seat
    freeSeat = takeFreeSeatUnsafe ctx

    seatedPlayer :: Player
    seatedPlayer = player
      & player'seatL ?~ freeSeat
      & player'statusL .~ SatOut

    gameCtx' :: GameCtx
    gameCtx' = ctx 
      & gameCtx'playersL %~ Map.insert freeSeat seatedPlayer

leaveSeat' :: Seat -> GameCtx -> GameCtx
leaveSeat' seat ctx = ctx 
  & gameCtx'playersL %~ Map.delete seat

fold' :: Seat -> GameCtx -> GameCtx
fold' seat ctx = ctx & gameCtx'playersL %~ Map.adjust foldPlayer seat
  where
    foldPlayer :: Player -> Player
    foldPlayer player = player
      & player'statusL .~ InHand Folded
      & player'betL .~ Chips 0
      & player'committedL .~ Chips 0

postAnte' :: Seat -> GameCtx -> GameCtx
postAnte' seat ctx@GameCtx{..} = ctx
  & gameCtx'playersL %~ Map.adjust postAntePlayer seat
  & gameCtx'potL .~ gameCtx'ante
  where
    postAntePlayer :: Player -> Player
    postAntePlayer player = player
      & player'chipsL -~ gameCtx'ante
      & player'committedL +~ gameCtx'ante
      & player'statusL .~ SatIn HasPostedAnte
      & player'betL .~ gameCtx'ante

bet' :: Seat -> Chips -> GameCtx -> GameCtx
bet' seat bet ctx@GameCtx{..} = ctx
  & gameCtx'playersL %~ Map.adjust betPlayer seat
  & gameCtx'betL .~ bet
  & gameCtx'potL +~ bet
  where
    bettingAction :: BettingAction
    bettingAction
      | gameCtx'bet == 0 && bet > 0 = MadeBet $ HasBet bet
      | bet == gameCtx'bet = MadeBet HasCalled
      | bet > gameCtx'bet = MadeBet $ HasRaised bet
      | otherwise = error "Invalid bet: bet is less than current bet"

    isAllIn :: Chips -> InHandStatus -> InHandStatus
    isAllIn chips status = case compare bet chips of
      EQ -> AllIn
      LT -> status
      GT -> error "Invalid bet: bet is greater than _player's chips"

    betPlayer :: Player -> Player
    betPlayer player@Player{..} = player
      & player'chipsL -~ bet
      & player'betL .~ bet
      & player'committedL +~ bet
      & player'statusL .~ InHand (isAllIn player'chips (CanAct $ pure bettingAction))

check' :: Seat -> GameCtx -> GameCtx
check' seat ctx = ctx & gameCtx'playersL %~ Map.adjust checkPlayer seat
  where
    checkPlayer :: Player -> Player
    checkPlayer player = player 
      & player'statusL .~ InHand (CanAct $ pure Checked)

draw' :: Seat -> DrawChoices -> GameCtx -> GameCtx
draw' seat drawSelection ctx@GameCtx{..} = ctx 
  & gameCtx'playersL %~ Map.adjust drawPlayer seat
  & gameCtx'deckL .~ remainingDeck
  where
    drawPlayer :: Player -> Player
    drawPlayer player = player & 
      player'handL .~ newHand

    playerHand :: Maybe Hand
    playerHand = Map.lookup seat gameCtx'players >>= player'hand

    mkNewHand :: Hand -> (Maybe Hand, Deck)
    mkNewHand = first Just . replaceCardsWithDraw gameCtx'deck drawSelection

    (newHand :: Maybe Hand, remainingDeck :: Deck) = maybe (playerHand, gameCtx'deck) mkNewHand playerHand

muckHand' :: Seat -> GameCtx -> GameCtx
muckHand' seat ctx = ctx & gameCtx'playersL %~ Map.adjust muckPlayer seat
  where
    muckPlayer :: Player -> Player
    muckPlayer player@Player{..}
      | player'status == InHand Folded = player
          & player'handL .~ Nothing
      | otherwise = player

sitIn' :: Seat -> GameCtx -> GameCtx
sitIn' seat ctx = ctx & gameCtx'playersL %~ Map.adjust sitInPlayer seat
  where
    sitInPlayer :: Player -> Player
    sitInPlayer player = player 
      & player'statusL .~ SatIn HasNotPostedAnte

sitOut' :: Seat -> GameCtx -> GameCtx
sitOut' seat ctx = ctx 
  & gameCtx'playersL %~ Map.adjust sitOutPlayer seat
  where
    sitOutPlayer :: Player -> Player
    sitOutPlayer player = player
      & player'statusL .~ SatOut
      & player'betL .~ Chips 0
      & player'committedL .~ Chips 0
      & player'handL .~ Nothing

endRound' :: GameCtx -> GameCtx
endRound' ctx@GameCtx{..} = case gameCtx'round of
  PostDrawRound -> payoutWinners
  _ -> moveToNextRound
  where
    moveToNextRound :: GameCtx
    moveToNextRound = ctx
      & gameCtx'playersL .~ Map.map updatePlayerNextRound gameCtx'players
      & gameCtx'roundL .~ nextRound gameCtx'round
      & gameCtx'betL .~ Chips 0

    updatePlayerNextRound :: Player -> Player
    updatePlayerNextRound player@Player{..} = player
      & player'betL .~ Chips 0
      & player'committedL .~ Chips 0
      & player'statusL .~ if player'status == InHand Folded || 
                            player'status == InHand AllIn
                        then player'status
                        else InHand (CanAct mempty)

    payoutWinners :: GameCtx
    payoutWinners = ctx
      & gameCtx'potL .~ Chips 0
      & gameCtx'anteL .~ Chips 0
      & gameCtx'betL .~ Chips 0
      & gameCtx'winnersL ?~ winners
      & gameCtx'playersL .~ Map.map updatePlayerPostDrawRound gameCtx'players
      & gameCtx'roundL .~ nextRound gameCtx'round

    updatePlayerPostDrawRound :: Player -> Player
    updatePlayerPostDrawRound player@Player{..} = player
      & player'betL .~ Chips 0
      & player'committedL .~ Chips 0
      & player'statusL .~ SatOut
      & player'chipsL .~ if maybe False (isWinner winners) player'seat
                        then player'chips + 
                             splitChips'chips splitChips +
                             if player'seat == playerNearestToLeftOfDealer ctx
                             then splitChips'oddChip splitChips
                             else Chips 0
                        else Chips 0

    splitChips :: SplitChips
    splitChips = splitChipsAmongWinners gameCtx'pot (winnersLength winners)

    winners :: Winners
    winners = case evaluatePlayerHands playerShowdownHands of
      [SeatHand{..}] -> SinglePlayerShowdown playerHand'seat
      multiPlayerShowdown -> MultiPlayerShowdown multiPlayerShowdown

    playerShowdownHands :: [SeatHand]
    playerShowdownHands = Map.elems $ Map.mapMaybe rankPlayerHand gameCtx'players