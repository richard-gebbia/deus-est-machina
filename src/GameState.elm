module GameState where

type GameState action 
    = GameState (action -> GameState action)

end : GameState action
end = 
    GameState (always end)
