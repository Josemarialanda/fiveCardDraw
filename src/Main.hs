module Main where

import           FiveCardDraw.FiveCardDraw (Game, bet, call, check, dealCards,
                                            designateDealer, draw, endRound,
                                            fold, leaveSeat, muckHand, postAnte,
                                            raise, run, sitIn, sitOut, takeSeat)
import           FiveCardDraw.Types        (Chips (Chips, unChips),
                                            DrawChoice (Discard, Keep),
                                            DrawChoices (..), GameCtx (..),
                                            GameF, Player, Seat (Seat1))
import           FiveCardDraw.Utils.Utils  (mkPlayer)
import           System.Random             (randomIO)
import           Text.Pretty.Simple        (pPrint)


-- TODO:
-- 4. If a player is left without enough chips to call then they are all in and can only call the amount they have left (or they can fold and forfeit the game)
--    When a player goes all-in and cannot match the full amount of a bet, they are still eligible to win the portion of the pot they contributed to up until they went all-in.
--    Any additional bets create separate side pots for those who can match them. The main pot is contested among players who are not all-in.

-- 5. If another player raises the bet, other players must call, raise or fold again.
--   The players must add the difference between their initial bet and the raised bet to the pot.
--   The betting round ends when all players have either called, checked, raised or folded.

main :: IO ()
main = runGame


exampleGame :: Game ()
exampleGame = do
  -- Setup round
  player1 <- takeSeat $ mkPlayer "Player 1" 1000
  player2 <- takeSeat $ mkPlayer "Player 2" 1000
  sitIn player1
  sitIn player2
  postAnte player1
  postAnte player2
  designateDealer player1
  dealCards
  endRound

  check player1
  bet player2 10
  call player1
  endRound

--   -- Setup round
--   player1 <- takeSeat $ mkPlayer "Player 1" 1000
--   player2 <- takeSeat $ mkPlayer "Player 2" 1000
--   sitIn player1
--   sitIn player2
--   postAnte player1
--   postAnte player2
--   designateDealer player1
--   dealCards
--   endRound
-- 
--   -- pre draw round
--   bet player1 10
--   call player2 
--   endRound
-- 
--   -- draw round 1
--   draw player1 $ DrawChoices Discard Discard Discard Discard Keep
--   draw player2 $ DrawChoices Discard Discard Discard Discard Keep
--   bet player1 10
--   call player2
--   endRound
-- 
--   -- draw round 2
--   draw player1 $ DrawChoices Discard Discard Discard Discard Keep
--   draw player2 $ DrawChoices Discard Discard Discard Discard Keep
--   bet player1 107
--   call player2
--   endRound

runGame :: IO ()
runGame = do
  let ante = 10
  randomSeed <- randomIO
  gameResult <- run randomSeed ante exampleGame
  case gameResult of
    Left err      -> error $ show err
    Right (_,GameCtx{..}) -> do
      pPrint $ "bet: " <> show (unChips gameCtx'bet) <> "\n"
      pPrint $ "pot: " <> show (unChips gameCtx'pot) <> "\n"
      putStrLn "Players:"
      pPrint gameCtx'players
