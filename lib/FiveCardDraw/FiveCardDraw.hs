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
  
import qualified FiveCardDraw.Actions as Actions
import qualified Data.Map             as Map

import FiveCardDraw.Utils.Utils   (mkGameFromSeed)
import FiveCardDraw.Types         (Chips, DrawChoices, GameError,
                                   GameF(..), GameCtx, Hand,
                                   Player, Seat)
import Control.Monad.Free         (iterM, liftF, Free, MonadFree)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Except       (MonadError, ExceptT, runExceptT)
import Control.Monad.State        (MonadState, StateT(..))

type Game = Free GameF

dealCards :: MonadFree GameF m => m ()
dealCards = liftF (DealCards ())

designateDealer :: MonadFree GameF m => Seat -> m ()
designateDealer a = liftF (DesignateDealer a ())

takeSeat :: MonadFree GameF m => Player -> m Seat
takeSeat a = liftF (TakeSeat a id)

leaveSeat :: MonadFree GameF m => Seat -> m ()
leaveSeat a = liftF (LeaveSeat a ())

postAnte :: MonadFree GameF m => Seat -> m ()
postAnte a = liftF (PostAnte a ())

fold :: MonadFree GameF m => Seat -> m ()
fold a = liftF (Fold a ())

call :: MonadFree GameF m => Seat -> m ()
call a = liftF (Call a ())

raise :: MonadFree GameF m => Seat -> Chips -> m ()
raise a b = liftF (Raise a b ())

bet :: MonadFree GameF m =>Seat -> Chips -> m ()
bet a b = liftF (Bet a b ())

check :: MonadFree GameF m => Seat -> m ()
check a = liftF (Check a ())

draw :: MonadFree GameF m => Seat -> DrawChoices -> m ()
draw a b = liftF (Draw a b ())

muckHand :: MonadFree GameF m => Seat -> m ()
muckHand a = liftF (MuckHand a ())

sitOut :: MonadFree GameF m => Seat -> m ()
sitOut a = liftF (SitOut a ())

sitIn :: MonadFree GameF m => Seat -> m ()
sitIn a = liftF (SitIn a ())

endRound :: MonadFree GameF m => m ()
endRound = liftF (EndRound ())

newtype Interpreter a = Interpreter
  { runInterpreter :: StateT GameCtx (ExceptT GameError IO) a }
  deriving (Functor, Applicative, Monad, MonadError GameError, MonadState GameCtx)

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
  EndRound      next                -> Actions.endRound next

interpret :: Game a -> Interpreter a
interpret = iterM game

run :: Int -> Int -> Game a -> IO (Either GameError (a, GameCtx))
run seed ante = runExceptT . flip runStateT (mkGameFromSeed ante seed) . runInterpreter . interpret