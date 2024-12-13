module FiveCardDraw.Types where

import           Control.Lens         (DefName (..), lensField, lensRules,
                                       makeLensesWith, (&), (.~))
import           Control.Monad.Except (MonadError)
import           Control.Monad.State  (MonadState)
import           Data.Function        (on)
import           Data.Map             (Map)
import           Data.Monoid          (Last (..))
import           Language.Haskell.TH  (mkName, nameBase)

-- | The 'Bundle' type is a type alias for the monad stack the game interperters need to run.
type Bundle m = (MonadState GameCtx m, MonadError GameError m)

-- | The 'Rank' type represents the rank of a card.
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

-- | The 'Suit' type represents the suit of a card.
data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades
  deriving (Eq, Read, Ord, Show, Bounded, Enum)

-- | The 'HandRank' type represents the rank of a hand.
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

-- | The 'Card' type represents a card in a deck of cards.
data Card = Card
  { card'rank :: Rank
  , card'suit :: Suit
  }
  deriving (Eq, Read, Show)

instance Ord Card where
  compare :: Card -> Card -> Ordering
  compare = compare `on` card'rank -- ^ compare two cards by their rank

-- | The 'DrawChoice' type represents the choice a player can make when drawing cards.
data DrawChoice = Discard | Keep
  deriving (Eq, Read, Ord, Show)

-- | A player can make up to five choices when drawing cards.
data DrawChoices = DrawChoices
  { draw'choice1 :: DrawChoice
  , draw'choice2 :: DrawChoice
  , draw'choice3 :: DrawChoice
  , draw'choice4 :: DrawChoice
  , draw'choice5 :: DrawChoice
  }
  deriving (Eq, Read, Ord, Show)

-- | The 'Hand' type represents a hand of five cards.
data Hand = Hand
  { hand'card1 :: Card
  , hand'card2 :: Card
  , hand'card3 :: Card
  , hand'card4 :: Card
  , hand'card5 :: Card
  }
  deriving (Eq, Read, Ord, Show)

-- | The 'RankedHand' type represents a hand with a rank.
data RankedHand = RankedHand
  { rankedHand'handRank :: HandRank
  , rankedHand'hand     :: Hand
  }
  deriving (Eq, Read, Ord, Show)

-- | The 'SeatHand' type represents the hand of a player at a seat.
data SeatHand = SeatHand
  { playerHand'rankedHand :: RankedHand
  , playerHand'seat       :: Seat
  }
  deriving (Eq, Read, Ord, Show)

-- | The 'Deck' type represents a deck of cards.
newtype Deck = Deck { unDeck :: [Card] }
  deriving (Eq, Read, Ord, Show)

 -- | The 'Chips' type represents integer values of chips.
newtype Chips = Chips { unChips :: Int }
  deriving (Eq, Read, Ord, Show)

-- | The 'SplitChips' type represents a split of chips into two parts:
-- * The chips that can be split evenly
-- * The odd chip that cannot be split evenly
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

-- | The 'GameCtx' type represents the context of a game.
data GameCtx = GameCtx
  { gameCtx'deck    :: Deck          -- ^ The deck of cards
  , gameCtx'pot     :: Chips         -- ^ The pot of chips
  , gameCtx'ante    :: Chips         -- ^ The ante of chips
  , gameCtx'round   :: Round         -- ^ The current round of the game
  , gameCtx'bet     :: Chips         -- ^ The current bet of chips
  , gameCtx'players :: Players       -- ^ The players in the game
  , gameCtx'dealer  :: Maybe Seat    -- ^ The dealer of the game
  , gameCtx'winners :: Maybe Winners -- ^ The winners of the game
  }
  deriving (Eq, Read, Ord, Show)

-- | Players are stored in a map with the seat as the key and the player as the value.
type Players = Map Seat Player

-- | The 'Seat' type represents a seat at a table (game).
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

-- | The 'Player' type represents a player in a game.
data Player = Player
  { player'name      :: String       -- ^ The name of the player
  , player'hand      :: Maybe Hand   -- ^ The hand of the player
  , player'chips     :: Chips        -- ^ The chips of the player
  , player'bet       :: Chips        -- ^ The current bet of the player
  , player'committed :: Chips        -- ^ The chips the player has committed to the pot in the current round
  , player'status    :: PlayerStatus -- ^ The status of the player in the game (sat out, sat in, in hand)
  , player'seat      :: Maybe Seat   -- ^ The seat the player is sitting at in the game (if any)
  }
  deriving (Eq, Read, Ord, Show)

-- | The 'PostedAnte' type represents whether a player has posted an ante or not.
data PostedAnte = HasPostedAnte | HasNotPostedAnte
  deriving (Eq, Read, Ord, Show)

-- | The 'PlayerStatus' type represents the status of a player in a game.
data PlayerStatus
  = SatOut              -- ^ The player is sitting out of the game (spectating)
  | SatIn PostedAnte    -- ^ The player is sitting in the game (playing but not yet in hand)
  | InHand InHandStatus -- ^ The player is in the hand (playing and in hand)
  deriving (Eq, Read, Ord, Show)

-- | The 'InHandStatus' type represents the status of a player in a hand.
data InHandStatus
  = CanAct (Last BettingAction) -- ^ The player can possibly still act in the current round:
                                --   * The player placed a bet but the bet was raised by another player
                                --   * The player checked but the bet was raised by another player
                                --   * The player has not yet acted in the current round
  | Folded                      -- ^ The player has folded in the current hand
  | AllIn                       -- ^ The player has gone all in in the current hand
  deriving (Eq, Read, Ord, Show)

-- | The 'BettingAction' type represents the betting action of a player.
data BettingAction 
  = MadeBet HasBet -- ^ The player made a bet (called, bet, or raised)
  | Checked        -- ^ The player checked
  deriving (Eq, Read, Ord, Show)

-- | The 'HasBet' type represents the non checked betting actions a player can make.
data HasBet 
  = HasCalled       -- ^ The player has called (matched the current bet)
  | HasBet Chips    -- ^ The player has placed a bet
  | HasRaised Chips -- ^ The player has raised the current bet
  deriving (Eq, Read, Ord, Show)

data Round 
  = SetupRound    -- ^ The setup round of the game
                  --   * Take seats (players)
                  --   * Sit in players
                  --   * Post ante
                  --   * Designate dealer
                  --   * Deal cards
  | PreDrawRound  -- ^ The pre draw round of the game. Players can:
                  --   * Bet
                  --   * Call
                  --   * Fold
                  --   * Check
  | DrawRound1    -- ^ The first draw round of the game. Players can
                  --   do everything they can in the pre draw round and also draw cards.
  | DrawRound2    -- ^ Another draw round of the game.
  | ShowdownRound -- Payoff the winners
  deriving (Eq, Read, Ord, Show, Bounded, Enum)

-- | The 'Winners' type represents the winners of a game.
data Winners
  = MultiPlayerShowdown { multiPlayerShowdown'playerHands :: [SeatHand] }
  -- ^ Multiple players have shown their hands and the winner(s) are determined by the hand ranks.
  | SinglePlayerShowdown { singlePlayerShowdown'seat :: Seat }
  -- ^ Only one player has not folded and is the winner.
  deriving (Eq, Read, Ord, Show)

-- | The 'GameF' type represents the actions a player can take in a game.
data GameF next =
    DealCards next                  -- ^ Deal cards to players 
  | DesignateDealer Seat next       -- ^ Designate the dealer
  | TakeSeat Player (Seat -> next)  -- ^ Take a seat at the table
  | LeaveSeat Seat next             -- ^ Leave a seat at the table
  | PostAnte Seat next              -- ^ Post an ante
  | Fold Seat next                  -- ^ Fold a hand
  | Call Seat next                  -- ^ Call a bet
  | Raise Seat Chips next           -- ^ Raise a bet
  | Bet Seat Chips next             -- ^ Bet chips
  | Check Seat next                 -- ^ Check a bet
  | Draw Seat DrawChoices next      -- ^ Draw cards
  | MuckHand Seat next              -- ^ Muck a hand
  | SitOut Seat next                -- ^ Sit out of the game
  | SitIn Seat next                 -- ^ Sit in the game
  | EndRound next                   -- ^ End the current round
  deriving (Functor)

-- | The 'GameError' type represents the errors that can occur in a game.
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
