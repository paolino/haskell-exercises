import Control.Arrow 

data T a = T a [T a] 

paths :: T a -> [(a,[Int])]
paths (T x ts) = (x,[]) : (zip [0..] ts >>= \(i,t) -> second (i:) <$> paths t)
