import Control.Applicative
import Control.Monad.List
import Control.Monad.State

liftLT :: Monad m => [a] -> ListT m a
liftLT = ListT . return

produceAndCountTuples :: [a] -> [b] -> ListT (State Int) (a,b)
produceAndCountTuples xs ys = do 
  t <- (,) <$> liftLT xs <*> liftLT ys
  modify (+1)
  return t

main = print $ runState (runListT $ produceAndCountTuples [False,True] [1..4]) 0
