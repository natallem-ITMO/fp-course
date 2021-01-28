{-# LANGUAGE Rank2Types       #-}
module Lenses.Lens_examples
where
import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Lens.Traversal
import Control.Monad.State.Lazy

data Point = Point{_x :: Double , _y :: Double}
    deriving Show

data Unit = Unit {_health :: Int, _position :: Point}
    deriving Show

data Game = Game {_score :: Int, _units :: [Unit], _boss :: Unit}
    deriving Show

initialState :: Game
initialState = Game
    { _score = 0
    , _units =
        [ Unit { _health = 10
               , _position = Point { _x = 3.5
                                   , _y = 7.0 }
               }
        , Unit { _health = 15
               , _position = Point { _x = 1.0
                                   , _y = 1.0 }
               }
        , Unit { _health = 8
               , _position = Point { _x = 0.0
                                   , _y = 2.1 }
               }
        ]
    , _boss = Unit
        { _health = 100
        , _position = Point { _x = 0.0
                            , _y = 0.0 }
        }
    }


y :: Lens' Point Double 
y = lens _y (\p x -> p {_y = x})

x :: Lens' Point Double 
x = lens _x (\p x -> p{_x = x})

health :: Lens' Unit Int
health = lens _health (\unit h -> unit{_health = h})

position :: Lens' Unit Point 
position = lens _position (\unit x -> unit{_position = x})

score :: Lens' Game Int
score = lens _score (\game x -> game{_score = x})

units :: Lens' Game [Unit]
units = lens _units (\game x -> game {_units = x})

boss :: Lens' Game Unit
boss = lens _boss (\game x -> game {_boss = x})

exTraversedView = toListOf (units.traversed.health ) initialState
exTraversedView2 = initialState^.. units.traversed.health

exGetterLens :: Double 
exGetterLens = initialState ^. boss.position.x

exSetterLens :: Game
exSetterLens =initialState & score .~ 10
exSetterLens2 :: Game
exSetterLens2 =initialState & boss.health %~ (+10)


strike :: StateT Game IO ()
strike = do
    liftIO $ putStrLn "strike!"
    boss.health -= 10

--newState <- execStateT strike initialState 
-- newState^.boss.health 

partyHP :: Traversal' Game Int
partyHP = units.traversed.health

getHPs :: [Int]
getHPs = initialState ^.. partyHP

around :: Point -> Double -> Traversal' Unit Unit
around center radius = filtered (\unit ->
   (unit^.position.x - center^.x)^2
  + (unit^.position.y - center^.y)^2
  < radius^2 )

fireBreath :: Point -> StateT Game IO ()
fireBreath point = do
    liftIO $ putStrLn  "Row!"
    units.traversed.around point 1.0.health -= 3

fireBreathTraversal :: StateT Game IO () 
fireBreathTraversal = do
    lift  $ putStrLn "*raw*"
    units.traversed.health -= 3

ttt :: Game -> Game
ttt = (.~) score 10

tupleView :: Integer
tupleView = (1,2,4) ^. _1 
tupleSet :: (Integer, Integer, Integer)
tupleSet = (1,3,4)  & _1 .~ 3
tupleOver :: (Integer, Integer, Integer)
tupleOver = (1,2,4) & _2 %~ (+2)

stateTuple :: StateT (Int,Int,Int) IO () 
stateTuple = do 
    _1 -= 20









