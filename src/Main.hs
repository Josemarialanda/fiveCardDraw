module Main where

import FiveCardDraw.FiveCardDraw (bet, call, dealCards,
                                  designateDealer, draw,
                                  endRound, fold, muckHand,
                                  postAnte, raise, run, sitIn,
                                  takeSeat, Game) 
import FiveCardDraw.Utils.Utils  (mkPlayer)
import Text.Pretty.Simple        (pPrint)
import System.Random             (randomIO)
import FiveCardDraw.Types        (Chips(Chips), DrawChoice(Keep, Discard),
                                  DrawChoices(..), GameF, Player, GameCtx (..))

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
  
  bet player1 (Chips 250)
  call player2

  endRound

  -- post draw round

  draw player1 $ DrawChoices Keep Keep Keep Discard Discard
  draw player2 $ DrawChoices Discard Discard Keep Discard Keep

  raise player1 (Chips 500)
  fold player2
  muckHand player2

  -- showdown

  endRound

runGame :: IO ()
runGame = do
  randomSeed <- randomIO
  gameResult <- run randomSeed exampleGame
  case gameResult of
    Left err -> error $ show err
    Right (_,ctx) -> do
      print "The winner(s) (is/are):"
      pPrint $ gameCtx'winners ctx
      putStrLn "Players:"
      pPrint $ gameCtx'players ctx

