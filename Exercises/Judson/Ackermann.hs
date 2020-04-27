
import Control.Monad.State

ackermann :: Int -> Int -> Int
ackermann 0 y = y + 1
ackermann x 0 = ackermann (x-1) 1
ackermann x y = ackermann (x-1) (ackermann x (y-1))

ackermannSteps :: Int -> Int -> State Int Int
ackermannSteps x y = modify (+1) >> case (x,y) of 
    (0,y) -> return (y + 1)
    (x,0) -> ackermannSteps (x-1) 1
    (x,y) -> ackermannSteps x (y-1) >>= ackermannSteps (x-1)
    
steps :: Int -> Int -> (Int,Int)
steps x y = runState (ackermannSteps x y) 0

