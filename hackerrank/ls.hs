import Control.Monad.List
import Control.Monad.State

countCalls :: ListT (State Int) (Int,Int)
countCalls = do
  a <- ListT . return $ [1..2]
  b <- ListT . return $ [1..2]
  modify (+1)
  return (a,b)
