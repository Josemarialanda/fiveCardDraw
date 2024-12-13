module FiveCardDraw.FiveCardDraw
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
  , run
  , Game
  ) where

import qualified Data.Map                   as Map
import qualified FiveCardDraw.Actions       as Actions

import           Control.Monad.Except       (ExceptT, MonadError, runExceptT)
import           Control.Monad.Free         (Free, MonadFree, iterM, liftF)
import           Control.Monad.State        (MonadState, StateT (..))
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           FiveCardDraw.Types         (Chips (..), DrawChoices, GameCtx (..),
                                             GameError, GameF (..), Hand,
                                             Player, Seat, Round (..), Deck (..), Card (..))
import           System.Random              (Random (randomR), RandomGen, mkStdGen)
import FiveCardDraw.Utils.Utils (shuffle)

type Game = Free GameF

-- | Deal cards to all players.
dealCards :: MonadFree GameF m => m ()
dealCards = liftF (DealCards ())

-- | Designate the dealer.
designateDealer :: MonadFree GameF m => Seat -> m ()
designateDealer a = liftF (DesignateDealer a ())

-- | Take a seat at the table.
takeSeat :: MonadFree GameF m => Player -> m Seat
takeSeat a = liftF (TakeSeat a id)

-- | Leave a seat at the table.
leaveSeat :: MonadFree GameF m => Seat -> m ()
leaveSeat a = liftF (LeaveSeat a ())

-- | Post the ante.
postAnte :: MonadFree GameF m => Seat -> m ()
postAnte a = liftF (PostAnte a ())

-- | Fold a hand.
fold :: MonadFree GameF m => Seat -> m ()
fold a = liftF (Fold a ())

-- | Call a bet.
call :: MonadFree GameF m => Seat -> m ()
call a = liftF (Call a ())

-- | Raise a bet.
raise :: MonadFree GameF m => Seat -> Chips -> m ()
raise a b = liftF (Raise a b ())

-- | Bet a number of chips.
bet :: MonadFree GameF m =>Seat -> Chips -> m ()
bet a b = liftF (Bet a b ())

-- | Check a hand.
check :: MonadFree GameF m => Seat -> m ()
check a = liftF (Check a ())

-- | Draw cards specifying which cards to discard.
draw :: MonadFree GameF m => Seat -> DrawChoices -> m ()
draw a b = liftF (Draw a b ())

-- | Muck a hand.
muckHand :: MonadFree GameF m => Seat -> m ()
muckHand a = liftF (MuckHand a ())

-- | Sit out a hand.
sitOut :: MonadFree GameF m => Seat -> m ()
sitOut a = liftF (SitOut a ())

-- | Sit in a hand.
sitIn :: MonadFree GameF m => Seat -> m ()
sitIn a = liftF (SitIn a ())

-- | End the round.
endRound :: MonadFree GameF m => m ()
endRound = liftF (EndRound ())

-- | The interpreter which runs the game.
newtype Interpreter a = Interpreter
  { runInterpreter :: StateT GameCtx (ExceptT GameError IO) a }
  deriving (Functor, Applicative, Monad, MonadError GameError, MonadState GameCtx)

-- | The actions the interpreter can take.
game :: GameF (Interpreter a) -> Interpreter a
game = \case
  DealCards       next                -> Actions.dealCards next
  DesignateDealer seat next           -> Actions.designateDealer seat next
  TakeSeat        player next         -> Actions.takeSeat player next
  LeaveSeat       player next         -> Actions.leaveSeat player next
  PostAnte        player next         -> Actions.postAnte player next
  Fold            player next         -> Actions.fold player next
  Call            player next         -> Actions.call player next
  Raise           player chips next   -> Actions.raise player chips next
  Bet             player chips next   -> Actions.bet player chips next
  Check           player next         -> Actions.check player next
  Draw            player drawSel next -> Actions.draw player drawSel next
  MuckHand        player next         -> Actions.muckHand player next
  SitOut          player next         -> Actions.sitOut player next
  SitIn           player next         -> Actions.sitIn player next
  EndRound        next                -> Actions.endRound next

-- | Interpret the game.
interpret :: Game a -> Interpreter a
interpret = iterM game

-- | Run the game.
run :: Int -> Int -> Game a -> IO (Either GameError (a, GameCtx))
run seed ante = runExceptT . flip runStateT (mkGameFromSeed ante seed) . runInterpreter . interpret

-- Helper functions

-- | Create a game context from a seed.
mkGameFromSeed :: Int -> Int -> FiveCardDraw.Types.GameCtx
mkGameFromSeed ante seed = GameCtx
  { gameCtx'deck = mkShuffledDeck seed
  , gameCtx'pot = Chips 0
  , gameCtx'ante = Chips ante
  , gameCtx'round = SetupRound
  , gameCtx'bet = FiveCardDraw.Types.Chips 0
  , gameCtx'dealer = Nothing
  , gameCtx'players = mempty
  , gameCtx'winners = Nothing
  }

-- | A standard deck of cards.
initialDeck :: Deck
initialDeck = Deck $ Card <$> [minBound ..] <*> [minBound ..]

-- | Get a shuffled deck of cards with a seed.
mkShuffledDeck :: Int -> Deck
mkShuffledDeck = shuffledDeck . mkStdGen

-- Get a shuffled deck of cards.
shuffledDeck :: RandomGen g => g -> Deck
shuffledDeck gen = Deck <$> fst $ shuffle gen (unDeck initialDeck)