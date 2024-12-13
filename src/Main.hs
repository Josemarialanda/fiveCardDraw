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

main :: IO ()
main = runGame


exampleGame :: Game ()
exampleGame = do
  -- Setup round
  player1 <- takeSeat $ mkPlayer "Player 1" 100
  player2 <- takeSeat $ mkPlayer "Player 2" 100
  player3 <- takeSeat $ mkPlayer "Player 3" 100
  sitIn player1
  sitIn player2
  sitIn player3
  postAnte player1
  postAnte player2
  postAnte player3
  designateDealer player1
  dealCards
  endRound

  bet player1 50
  raise player2 60
  fold player1
  call player3
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
      pPrint $ "ante: " <> show (unChips gameCtx'ante) <> "\n"
      pPrint $ "bet: " <> show (unChips gameCtx'bet) <> "\n"
      pPrint $ "pot (ante per player + bets): " <> show (unChips gameCtx'pot) <> "\n"
      putStrLn "Players:"
      pPrint gameCtx'players
      putStrLn "Winners:"
      pPrint gameCtx'winners
