module Main where

import FiveCardDraw.FiveCardDraw (bet, call, dealCards,
                                  designateDealer, draw,
                                  endRound, fold, muckHand,
                                  postAnte, raise, run, sitIn,
                                  takeSeat, Game, leaveSeat, check) 
import FiveCardDraw.Utils.Utils  (mkPlayer)
import Text.Pretty.Simple        (pPrint)
import System.Random             (randomIO)
import FiveCardDraw.Types        (Chips(Chips), DrawChoice(Keep, Discard),
                                  DrawChoices(..), GameF, Player, GameCtx (..))


-- TODO: 
-- 1. If player checks and then another player places bet then the first player needs to call, raise or fold
-- 2. If a player raises then the other player needs to call, raise or fold
-- 3. If all players fold except one then the last player wins

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
  let player1Name = "Player 1"
      player2Name = "Player 2"

  player1 <- takeSeat $ mkPlayer player1Name 1000
  player2 <- takeSeat $ mkPlayer player2Name 1000

  -- pre draw round

  sitIn player1
  sitIn player2 
    
  postAnte player1
  postAnte player2
    
  designateDealer player1
  
  dealCards
  
  bet player1 (Chips 100)
  -- raise player2 (Chips 101)


  endRound
-- 
--   -- post draw round
-- 
--   draw player1 $ DrawChoices Keep Keep Keep Discard Discard
--   draw player2 $ DrawChoices Discard Discard Keep Discard Keep
-- 
--   raise player1 (Chips 500)
--   fold player2
--   muckHand player2
-- 
--   -- showdown
-- 
--   endRound

runGame :: IO ()
runGame = do
  let ante = 0
  randomSeed <- randomIO
  gameResult <- run randomSeed ante exampleGame
  case gameResult of
    Left err -> error $ show err
    Right (_,ctx) -> pPrint ctx