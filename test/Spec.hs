import           Control.Monad             (void)
import qualified Data.Map.Strict           as Map
import           FiveCardDraw.FiveCardDraw (Game, bet, call, check, dealCards,
                                            designateDealer, draw, endRound,
                                            fold, leaveSeat, muckHand, postAnte,
                                            raise, run, sitIn, takeSeat)
import           FiveCardDraw.Types        (Chips (Chips),
                                            DrawChoice (Discard, Keep),
                                            DrawChoices (..), GameCtx (..),
                                            GameError (..), GameF, Player (..),
                                            PlayerStatus (SatIn),
                                            PostedAnte (HasNotPostedAnte),
                                            Seat (..))
import           FiveCardDraw.Utils.Utils  (mkPlayer)
import           System.Random
import           Test.Tasty
import           Test.Tasty.HUnit

-- run all tests
main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ takeSeatsTests
  , leaveSeatTests
  , postAnteTests
  , designateDealerTests
  , dealCardsTests
  -- , endRoundTests
  , checkTests
  , betTests
  , callTests
  , foldTests
  , raiseTests
  -- , drawTests
  -- , muckHandTests
  ]

dealCardsTests :: TestTree
dealCardsTests = testGroup "Deal Cards Tests"
  [ testCase "Cannot deal cards, less than 2 players sat in hand" $ do
      result <- run 0 0 dealCardsWithLessThan2SatInPlayers
      assertEqual
        "dealCardsWithLessThan2Players should return NotEnoughSatInPlayers"
        (Left NotEnoughSatInPlayers)
        result
  , testCase "Cannot deal cards, not all sat in players have posted ante" $ do
      result <- run 0 0 dealCardsWithPlayersNotPostedAnte
      assertEqual
        "dealCardsWithPlayersNotPostedAnte should return NotAllSatInPlayersHavePostedAnte"
        (Left NotAllSatInPlayersHavePostedAnte)
        result
  , testCase "Cannot deal cards, hand has no designated dealer" $ do
      result <- run 0 0 dealCardsWithNoDesignatedDealer
      assertEqual
        "dealCardsWithNoDesignatedDealer should return HandHasNoDesignatedDealer"
        (Left HandHasNoDesignatedDealer)
        result
  ]

dealCardsWithLessThan2SatInPlayers :: Game ()
dealCardsWithLessThan2SatInPlayers = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  sitIn player1
  dealCards

dealCardsWithPlayersNotPostedAnte :: Game ()
dealCardsWithPlayersNotPostedAnte = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  player2 <- takeSeat $ mkPlayer "Player 2" 1000
  sitIn player1
  sitIn player2
  postAnte player1
  dealCards

dealCardsWithNoDesignatedDealer :: Game ()
dealCardsWithNoDesignatedDealer = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  player2 <- takeSeat $ mkPlayer "Player 2" 1000
  sitIn player1
  sitIn player2
  postAnte player1
  postAnte player2
  dealCards

designateDealerTests :: TestTree
designateDealerTests = testGroup "Designate Dealer Tests"
  [ testCase "Cannot designate dealer, player not sat in" $ do
      result <- run 0 0 designateDealerWithPlayerNotSatIn
      assertEqual
        "designateDealerWithPlayerNotSatIn should return PlayerNotSatIn"
        (Left PlayerNotSatIn)
        result
  , testCase "Cannot designate dealer, player has not posted ante" $ do
      result <- run 0 0 designateDealerWithPlayerNotPostedAnte
      assertEqual
        "designateDealerWithPlayerNotPostedAnte should return PlayerHasNotPostedAnte"
        (Left PlayerHasNotPostedAnte)
        result
  ]

designateDealerWithPlayerNotSatIn :: Game ()
designateDealerWithPlayerNotSatIn = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  designateDealer player1

designateDealerWithPlayerNotPostedAnte :: Game ()
designateDealerWithPlayerNotPostedAnte = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  sitIn player1
  designateDealer player1

takeSeatsTests :: TestTree
takeSeatsTests = testGroup "Take Seats Tests"
  [ testCase "The same player cannot take seat twice" $ do
      result <- run 0 0 takeSeatTwice
      assertEqual
        "takeSeatTwice should return PlayerAlreadyHasSeat"
        (Left PlayerAlreadyHasSeat)
        result
  , testCase "A maximum of 6 players can take a seat" $ do
      result <- run 0 0 takeMoreThan6Seats
      assertEqual
        "takeMoreThan6Seats should return NoFreeSeatsAvailable"
        (Left NoFreeSeatsAvailable)
        result
  ]

takeSeatTwice :: Game ()
takeSeatTwice = do
  let player1 = mkPlayer "Player 1" 1000
  takeSeat player1
  takeSeat player1
  pure ()

takeMoreThan6Seats :: Game ()
takeMoreThan6Seats = do
  takeSeat $ mkPlayer "Player 1" 1000
  takeSeat $ mkPlayer "Player 2" 1000
  takeSeat $ mkPlayer "Player 3" 1000
  takeSeat $ mkPlayer "Player 4" 1000
  takeSeat $ mkPlayer "Player 5" 1000
  takeSeat $ mkPlayer "Player 6" 1000
  takeSeat $ mkPlayer "Player 7" 1000
  pure ()

leaveSeatTests :: TestTree
leaveSeatTests = testGroup "Leave Seat Tests"
  [ testCase "Cannot leave seat, no player at seat" $ do
      result <- run 0 0 leaveSeatWithNoPlayerAtSeat
      assertEqual
        "leaveSeatWithNoPlayerAtSeat should return NoPlayerAtSeat"
        (Left NoPlayerAtSeat)
        result
  , testCase "Cannot leave seat, player at seat not sat out" $ do
      result <- run 0 0 leaveSeatWithPlayerNotSatOut
      assertEqual
        "leaveSeatWithPlayerNotSatOut should return PlayerNotSatOut"
        (Left PlayerNotSatOut)
        result
  ]

leaveSeatWithNoPlayerAtSeat :: Game ()
leaveSeatWithNoPlayerAtSeat = do
  leaveSeat Seat1

leaveSeatWithPlayerNotSatOut :: Game ()
leaveSeatWithPlayerNotSatOut = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  sitIn player1
  leaveSeat player1

postAnteTests :: TestTree
postAnteTests = testGroup "Post Ante Tests"
  [ testCase "Cannot post ante, player not sat in" $ do
      result <- run 0 0 postAnteWithPlayerNotSatIn
      assertEqual
        "postAnteWithPlayerNotSatIn should return PlayerNotSatIn"
        (Left PlayerNotSatIn)
        result
  , testCase "Cannot post ante, player has already posted ante" $ do
      result <- run 0 0 postAnteWithPlayerHasPostedAnte
      assertEqual
        "postAnteWithPlayerHasPostedAnte should return PlayerHasPostedAnte"
        (Left PlayerHasPostedAnte)
        result
  , testCase "Cannot post ante, player has insufficient chips" $ do
      result <- run 0 10 postAnteWithPlayerHasInsufficientChips
      assertEqual
        "postAnteWithPlayerHasInsufficientChips should return InsufficientChips"
        (Left InsufficientChips)
        result
  ]

postAnteWithPlayerNotSatIn :: Game ()
postAnteWithPlayerNotSatIn = do
  postAnte Seat1

postAnteWithPlayerHasPostedAnte :: Game ()
postAnteWithPlayerHasPostedAnte = do
  player1 <- takeSeat $ mkPlayer "Player 1" 0
  sitIn player1
  postAnte player1
  postAnte player1

postAnteWithPlayerHasInsufficientChips :: Game ()
postAnteWithPlayerHasInsufficientChips = do
  player1 <- takeSeat $ mkPlayer "Player 1" 0
  sitIn player1
  postAnte player1

foldTests :: TestTree
foldTests = testGroup "Fold Tests"
  [ testCase "Cannot fold, player has already acted this round" $ do
      result <- run 0 0 foldWithPlayerHasLastBettingAction
      assertEqual
        "foldWithPlayerHasLastBettingAction should return PlayerHasLastBettingAction"
        (Left PlayerHasLastBettingAction)
        result
  , testCase "Cannot fold, player not in hand" $ do
      result <- run 0 0 foldWithPlayerNotInHand
      assertEqual
        "foldWithPlayerNotInHand should return PlayerNotInHand"
        (Left PlayerNotInHand)
        result
  , testCase "Cannot fold, player cannot act" $ do
      result <- run 0 0 foldWithPlayerCannotAct
      assertEqual
        "foldWithPlayerCannotAct should return PlayerCannotAct"
        (Left PlayerCannotAct)
        result
  ]

foldWithPlayerHasLastBettingAction :: Game ()
foldWithPlayerHasLastBettingAction = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  player2 <- takeSeat $ mkPlayer "Player 2" 1000
  sitIn player1
  sitIn player2
  postAnte player1
  postAnte player2
  endRound
  bet player1 10
  fold player1

foldWithPlayerNotInHand :: Game ()
foldWithPlayerNotInHand = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  player2 <- takeSeat $ mkPlayer "Player 2" 1000
  sitIn player1
  sitIn player2
  postAnte player1
  postAnte player2
  bet player1 10

foldWithPlayerCannotAct :: Game ()
foldWithPlayerCannotAct = undefined

callTests :: TestTree
callTests = testGroup "Call Tests"
  [ testCase "Cannot call, player has already acted this round" $ do
      result <- run 0 0 callWithPlayerHasLastBettingAction
      assertEqual
        "callWithPlayerHasLastBettingAction should return PlayerHasLastBettingAction"
        (Left PlayerHasLastBettingAction)
        result
  , testCase "Cannot call, player not in hand" $ do
      result <- run 0 0 callWithPlayerNotInHand
      assertEqual
        "callWithPlayerNotInHand should return PlayerNotInHand"
        (Left PlayerNotInHand)
        result
  , testCase "Cannot call, player cannot act" $ do
      result <- run 0 0 callWithPlayerCannotAct
      assertEqual
        "callWithPlayerCannotAct should return PlayerCannotAct"
        (Left PlayerCannotAct)
        result
  , testCase "Cannot call, no bet placed" $ do
      result <- run 0 0 callWithNoBetPlaced
      assertEqual
        "callWithNoBetPlaced should return NoBetPlaced"
        (Left NoBetPlaced)
        result
  , testCase "Cannot call, player has insufficient chips" $ do
      result <- run 0 10 callWithPlayerHasInsufficientChips
      assertEqual
        "callWithPlayerHasInsufficientChips should return InsufficientChips"
        (Left InsufficientChips)
        result
  ]

callWithPlayerHasLastBettingAction :: Game ()
callWithPlayerHasLastBettingAction = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  player2 <- takeSeat $ mkPlayer "Player 2" 1000
  sitIn player1
  sitIn player2
  postAnte player1
  postAnte player2
  endRound
  bet player1 10
  call player1

callWithPlayerNotInHand :: Game ()
callWithPlayerNotInHand = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  player2 <- takeSeat $ mkPlayer "Player 2" 1000
  sitIn player1
  sitIn player2
  postAnte player1
  postAnte player2
  call player1

callWithPlayerCannotAct :: Game ()
callWithPlayerCannotAct = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  player2 <- takeSeat $ mkPlayer "Player 2" 1000
  sitIn player1
  sitIn player2
  postAnte player1
  postAnte player2
  endRound
  bet player1 10
  fold player2
  call player2

callWithNoBetPlaced :: Game ()
callWithNoBetPlaced = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  player2 <- takeSeat $ mkPlayer "Player 2" 1000
  sitIn player1
  sitIn player2
  postAnte player1
  postAnte player2
  endRound
  call player1

callWithPlayerHasInsufficientChips :: Game ()
callWithPlayerHasInsufficientChips = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  player2 <- takeSeat $ mkPlayer "Player 2" 10
  sitIn player1
  sitIn player2
  postAnte player1
  postAnte player2
  endRound
  bet player1 100
  call player2

raiseTests :: TestTree
raiseTests = testGroup "Raise Tests"
  [ testCase "Cannot raise, player has already acted this round" $ do
      result <- run 0 0 raiseWithPlayerHasLastBettingAction
      assertEqual
        "raiseWithPlayerHasLastBettingAction should return PlayerHasLastBettingAction"
        (Left PlayerHasLastBettingAction)
        result
  , testCase "Cannot raise, player not in hand" $ do
      result <- run 0 0 raiseWithPlayerNotInHand
      assertEqual
        "raiseWithPlayerNotInHand should return PlayerNotInHand"
        (Left PlayerNotInHand)
        result
  , testCase "Cannot raise, player cannot act" $ do
      result <- run 0 0 raiseWithPlayerCannotAct
      assertEqual
        "raiseWithPlayerCannotAct should return PlayerCannotAct"
        (Left PlayerCannotAct)
        result
  , testCase "Cannot raise, player has insufficient chips" $ do
      result <- run 0 10 raiseWithPlayerHasInsufficientChips
      assertEqual
        "raiseWithPlayerHasInsufficientChips should return InsufficientChips"
        (Left InsufficientChips)
        result
  , testCase "Cannot raise, raise is not higher than current bet" $ do
      result <- run 0 0 raiseWithRaiseNotHigherThanCurrentBet
      assertEqual
        "raiseWithRaiseNotHigherThanCurrentBet should return RaiseNotHigherThanCurrentBet"
        (Left RaiseNotHigherThanCurrentBet)
        result
  , testCase "Cannot raise, no bet placed" $ do
      result <- run 0 0 raiseWithNoBetPlaced
      assertEqual
        "raiseWithNoBetPlaced should return NoBetPlaced"
        (Left NoBetPlaced)
        result
  ]

raiseWithPlayerHasLastBettingAction :: Game ()
raiseWithPlayerHasLastBettingAction = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  player2 <- takeSeat $ mkPlayer "Player 2" 1000
  sitIn player1
  sitIn player2
  postAnte player1
  postAnte player2
  endRound
  bet player1 10
  raise player1 10

raiseWithPlayerNotInHand :: Game ()
raiseWithPlayerNotInHand = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  player2 <- takeSeat $ mkPlayer "Player 2" 1000
  sitIn player1
  sitIn player2
  postAnte player1
  postAnte player2
  raise player1 10

raiseWithPlayerCannotAct :: Game ()
raiseWithPlayerCannotAct = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  player2 <- takeSeat $ mkPlayer "Player 2" 1000
  sitIn player1
  sitIn player2
  postAnte player1
  postAnte player2
  endRound
  bet player1 10
  fold player2
  raise player2 11

raiseWithPlayerHasInsufficientChips :: Game ()
raiseWithPlayerHasInsufficientChips = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  player2 <- takeSeat $ mkPlayer "Player 2" 10
  sitIn player1
  sitIn player2
  postAnte player1
  postAnte player2
  endRound
  bet player1 100
  raise player2 101

raiseWithRaiseNotHigherThanCurrentBet :: Game ()
raiseWithRaiseNotHigherThanCurrentBet = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  player2 <- takeSeat $ mkPlayer "Player 2" 1000
  sitIn player1
  sitIn player2
  postAnte player1
  postAnte player2
  endRound
  bet player1 100
  raise player2 99

raiseWithNoBetPlaced :: Game ()
raiseWithNoBetPlaced = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  player2 <- takeSeat $ mkPlayer "Player 2" 1000
  sitIn player1
  sitIn player2
  postAnte player1
  postAnte player2
  endRound
  raise player1 10

betTests :: TestTree
betTests = testGroup "Bet Tests"
  [ testCase "Cannot bet, player has already acted this round" $ do
      result <- run 0 0 betWithPlayerHasLastBettingAction
      assertEqual
        "betWithPlayerHasLastBettingAction should return PlayerHasLastBettingAction"
        (Left PlayerHasLastBettingAction)
        result
  , testCase "Cannot bet, player not in hand" $ do
      result <- run 0 0 betWithPlayerNotInHand
      assertEqual
        "betWithPlayerNotInHand should return PlayerNotInHand"
        (Left PlayerNotInHand)
        result
  , testCase "Cannot bet, player cannot act" $ do
      result <- run 0 0 betWithPlayerCannotAct
      assertEqual
        "betWithPlayerCannotAct should return PlayerCannotAct"
        (Left PlayerCannotAct)
        result
  , testCase "Cannot bet, player has insufficient chips" $ do
      result <- run 0 10 betWithPlayerHasInsufficientChips
      assertEqual
        "betWithPlayerHasInsufficientChips should return InsufficientChips"
        (Left InsufficientChips)
        result
  , testCase "Cannot bet, bet has already been placed" $ do
      result <- run 0 0 betWithBetAlreadyPlaced
      assertEqual
        "betWithBetAlreadyPlaced should return BetPlaced"
        (Left BetPlaced)
        result
  ]

betWithPlayerHasLastBettingAction :: Game ()
betWithPlayerHasLastBettingAction = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  player2 <- takeSeat $ mkPlayer "Player 2" 1000
  sitIn player1
  sitIn player2
  postAnte player1
  postAnte player2
  endRound
  bet player1 10
  bet player1 10

betWithPlayerNotInHand :: Game ()
betWithPlayerNotInHand = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  sitIn player1
  postAnte player1
  bet player1 10

betWithPlayerCannotAct :: Game ()
betWithPlayerCannotAct = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  player2 <- takeSeat $ mkPlayer "Player 2" 10
  sitIn player1
  sitIn player2
  postAnte player1
  postAnte player2
  endRound
  fold player1
  bet player1 10

betWithPlayerHasInsufficientChips :: Game ()
betWithPlayerHasInsufficientChips = do
  player1 <- takeSeat $ mkPlayer "Player 1" 10
  player2 <- takeSeat $ mkPlayer "Player 2" 10
  sitIn player1
  sitIn player2
  postAnte player1
  postAnte player2
  endRound
  bet player1 100

betWithBetAlreadyPlaced :: Game ()
betWithBetAlreadyPlaced = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  player2 <- takeSeat $ mkPlayer "Player 2" 1000
  sitIn player1
  sitIn player2
  postAnte player1
  postAnte player2
  endRound
  bet player1 10
  bet player2 10

checkTests :: TestTree
checkTests = testGroup "Check Tests"
  [ testCase "Cannot check, player has already acted this round" $ do
      result <- run 0 0 checkWithPlayerHasLastBettingAction
      assertEqual
        "checkWithPlayerHasLastBettingAction should return PlayerHasLastBettingAction"
        (Left PlayerHasLastBettingAction)
        result
  , testCase "Cannot check, player not in hand" $ do
      result <- run 0 0 checkWithPlayerNotInHand
      assertEqual
        "checkWithPlayerNotInHand should return PlayerNotInHand"
        (Left PlayerNotInHand)
        result
  , testCase "Cannot check, player cannot act" $ do
      result <- run 0 0 checkWithPlayerCannotAct
      assertEqual
        "checkWithPlayerCannotAct should return PlayerCannotAct"
        (Left PlayerCannotAct)
        result
  , testCase "Cannot check, bet has already been placed" $ do
      result <- run 0 0 checkWithBetAlreadyPlaced
      assertEqual
        "checkWithBetAlreadyPlaced should return BetPlaced"
        (Left BetPlaced)
        result
  ]

checkWithPlayerHasLastBettingAction :: Game ()
checkWithPlayerHasLastBettingAction = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  player2 <- takeSeat $ mkPlayer "Player 2" 1000
  sitIn player1
  sitIn player2
  postAnte player1
  postAnte player2
  endRound
  bet player1 10
  check player1

checkWithPlayerNotInHand :: Game ()
checkWithPlayerNotInHand = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  player2 <- takeSeat $ mkPlayer "Player 2" 1000
  sitIn player1
  sitIn player2
  postAnte player1
  postAnte player2
  check player1

checkWithPlayerCannotAct :: Game ()
checkWithPlayerCannotAct = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  player2 <- takeSeat $ mkPlayer "Player 2" 10
  sitIn player1
  sitIn player2
  postAnte player1
  postAnte player2
  endRound
  fold player1
  check player1

checkWithBetAlreadyPlaced :: Game ()
checkWithBetAlreadyPlaced = do
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  player2 <- takeSeat $ mkPlayer "Player 2" 1000
  sitIn player1
  sitIn player2
  postAnte player1
  postAnte player2
  endRound
  bet player1 10
  check player2
