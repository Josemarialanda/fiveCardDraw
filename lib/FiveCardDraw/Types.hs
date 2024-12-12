module FiveCardDraw.Types where

import           Control.Lens         (DefName (..), lensField, lensRules,
                                       makeLensesWith, (&), (.~))
import           Control.Monad.Except (MonadError)
import           Control.Monad.State  (MonadState)
import           Data.Function        (on)
import           Data.Map             (Map)
import           Data.Monoid          (Last (..))
import           Language.Haskell.TH  (mkName, nameBase)

data Rank
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Eq, Read, Ord, Show, Bounded, Enum)

data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades
  deriving (Eq, Read, Ord, Show, Bounded, Enum)

data HandRank
  = HighCard
  | Pair
  | TwoPair
  | ThreeOfAKind
  | Straight
  | Flush
  | FullHouse
  | FourOfAKind
  | StraightFlush
  deriving (Eq, Read, Ord, Show, Bounded, Enum)

data Card = Card
  { card'rank :: Rank
  , card'suit :: Suit
  }
  deriving (Eq, Read, Show)

instance Ord Card where
  compare :: Card -> Card -> Ordering
  compare = compare `on` card'rank

data DrawChoice = Discard | Keep
  deriving (Eq, Read, Ord, Show)

data DrawChoices = DrawChoices
  { draw'choice1 :: DrawChoice
  , draw'choice2 :: DrawChoice
  , draw'choice3 :: DrawChoice
  , draw'choice4 :: DrawChoice
  , draw'choice5 :: DrawChoice
  }
  deriving (Eq, Read, Ord, Show)

data Hand = Hand
  { hand'card1 :: Card
  , hand'card2 :: Card
  , hand'card3 :: Card
  , hand'card4 :: Card
  , hand'card5 :: Card
  }
  deriving (Eq, Read, Ord, Show)

data RankedHand = RankedHand
  { rankedHand'handRank :: HandRank
  , rankedHand'hand     :: Hand
  }
  deriving (Eq, Read, Ord, Show)

data SeatHand = SeatHand
  { playerHand'rankedHand :: RankedHand
  , playerHand'seat       :: Seat
  }
  deriving (Eq, Read, Ord, Show)

newtype Deck = Deck { unDeck :: [Card] }
  deriving (Eq, Read, Ord, Show)

newtype Chips = Chips { unChips :: Int }
  deriving (Eq, Read, Ord, Show)

data SplitChips = SplitChips
  { splitChips'chips   :: Chips
  , splitChips'oddChip :: Chips
  }
  deriving (Eq, Read, Ord, Show)

instance Semigroup Chips where
  (<>) :: Chips -> Chips -> Chips
  Chips a <> Chips b = Chips (a + b)
instance Num Chips where
  (+) :: Chips -> Chips -> Chips
  Chips a + Chips b = Chips (a + b)

  (-) :: Chips -> Chips -> Chips
  Chips a - Chips b = Chips (a - b)

  (*) :: Chips -> Chips -> Chips
  Chips a * Chips b = Chips (a * b)

  abs :: Chips -> Chips
  abs (Chips a) = Chips (abs a)

  signum :: Chips -> Chips
  signum (Chips a) = Chips (signum a)

  fromInteger :: Integer -> Chips
  fromInteger = Chips . fromInteger

data GameCtx = GameCtx
  { gameCtx'deck    :: Deck
  , gameCtx'pot     :: Chips
  , gameCtx'ante    :: Chips
  , gameCtx'round   :: Round
  , gameCtx'bet     :: Chips
  , gameCtx'players :: Players
  , gameCtx'dealer  :: Maybe Seat
  , gameCtx'winners :: Maybe Winners
  }
  deriving (Eq, Read, Ord, Show)

type Players = Map Seat Player

data Seat =
    Seat1
  | Seat2
  | Seat3
  | Seat4
  | Seat5
  | Seat6
  deriving (Eq, Read, Ord, Bounded, Enum)

instance Show Seat where
  show :: Seat -> String
  show Seat1 = "1"
  show Seat2 = "2"
  show Seat3 = "3"
  show Seat4 = "4"
  show Seat5 = "5"
  show Seat6 = "6"

data Player = Player
  { player'name      :: String
  , player'hand      :: Maybe Hand
  , player'chips     :: Chips
  , player'bet       :: Chips
  , player'committed :: Chips
  , player'status    :: PlayerStatus
  , player'seat      :: Maybe Seat
  }
  deriving (Eq, Read, Ord, Show)

data PostedAnte = HasPostedAnte | HasNotPostedAnte
  deriving (Eq, Read, Ord, Show)

data PlayerStatus
  = SatOut
  | SatIn PostedAnte
  | InHand InHandStatus
  deriving (Eq, Read, Ord, Show)

data InHandStatus
  = CanAct (Last BettingAction)
  | Folded
  | AllIn
  deriving (Eq, Read, Ord, Show)

data BettingAction = MadeBet HasBet | Checked
  deriving (Eq, Read, Ord, Show)

data HasBet = HasCalled | HasBet Chips | HasRaised Chips
  deriving (Eq, Read, Ord, Show)

data Round = SetupRound | PreDrawRound | DrawRound1 | DrawRound2 | ShowdownRound
  deriving (Eq, Read, Ord, Show, Bounded, Enum)

data Winners
  = MultiPlayerShowdown { multiPlayerShowdown'playerHands :: [SeatHand] }
  | SinglePlayerShowdown { singlePlayerShowdown'seat :: Seat }
  deriving (Eq, Read, Ord, Show)

data GameF next =
    DealCards next
  | DesignateDealer Seat next
  | TakeSeat Player (Seat -> next)
  | LeaveSeat Seat next
  | PostAnte Seat next
  | Fold Seat next
  | Call Seat next
  | Raise Seat Chips next
  | Bet Seat Chips next
  | Check Seat next
  | Draw Seat DrawChoices next
  | MuckHand Seat next
  | SitOut Seat next
  | SitIn Seat next
  | EndRound next
  deriving (Functor)

data GameError
  = DeckIncomplete
  | NotEnoughSatInPlayers
  | HandHasNoDesignatedDealer
  | NotAllSatInPlayersHavePostedAnte
  | PlayerNotSatIn
  | PlayerInHand
  | CanOnlyPerformOperationInSetupRound
  | PlayerNotInHand
  | NotInDrawRound
  | PlayerAlreadyHasSeat
  | NoPlayerAtSeat
  | PlayerHasPostedAnte
  | PlayerHasNotPostedAnte
  | InsufficientChips
  | RaiseNotHigherThanCurrentBet
  | PlayerCannotAct
  | PlayerCanStillAct
  | NotAllPlayersHaveActed
  | BetPlacedWithCheckedPlayers
  | PlayerHasBetLowerThanRoundBet
  | BetPlaced
  | NoBetPlaced
  | CardsNotDealt
  | NoFreeSeatsAvailable
  | PlayerNotSatOut
  | PlayerHasLastBettingAction
  | InvalidBetAmount
  deriving (Eq, Read, Ord, Show)

makeLensesWith (lensRules & lensField .~ \_ _ name -> [TopName (mkName (nameBase name <> "L"))]) ''Player
makeLensesWith (lensRules & lensField .~ \_ _ name -> [TopName (mkName (nameBase name <> "L"))]) ''GameCtx
