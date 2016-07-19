{-# language TypeFamilies #-}

module Closures where


-- closures hide something, sometimes it's good to expose the hidden stuff, call it de-serialization 

class Closure a where
    type Expose a 
    fromExpose :: Expose a -> a
    toExpose :: a -> Expose a





