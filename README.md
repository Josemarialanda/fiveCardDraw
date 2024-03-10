Five card draw is a simple poker style game. This library provides the basic functionality to play the game.

```haskell
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
```

```
🔥 cabal run
"The winner(s) (is/are):"
Just
    ( SinglePlayerShowdown { singlePlayerShowdown'seat = Seat0 } )
Players:
fromList
    [
        ( Seat0
        , Player
            { player'name = "Player 1"
            , player'hand = Just
                ( Hand
                    { hand'card1 = Card
                        { card'rank = Two
                        , card'suit = Hearts
                        }
                    , hand'card2 = Card
                        { card'rank = Ten
                        , card'suit = Hearts
                        }
                    , hand'card3 = Card
                        { card'rank = Three
                        , card'suit = Diamonds
                        }
                    , hand'card4 = Card
                        { card'rank = Seven
                        , card'suit = Spades
                        }
                    , hand'card5 = Card
                        { card'rank = Eight
                        , card'suit = Hearts
                        }
                    }
                )
            , player'chips = Chips
                { unChips = 1250 }
            , player'bet = Chips
                { unChips = 0 }
            , player'committed = Chips
                { unChips = 0 }
            , player'status = SatOut
            , player'seat = Just Seat0
            }
        )
    ,
        ( Seat1
        , Player
            { player'name = "Player 2"
            , player'hand = Nothing
            , player'chips = Chips
                { unChips = 0 }
            , player'bet = Chips
                { unChips = 0 }
            , player'committed = Chips
                { unChips = 0 }
            , player'status = SatOut
            , player'seat = Just Seat1
            }
        )
    ]
```