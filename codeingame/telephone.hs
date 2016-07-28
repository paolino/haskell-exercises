import qualified Data.Map as M

data T = T (M.Map Char T) | E deriving Show

merge :: T -> T -> T
merge E t = t
merge t E = t
merge (T t') (T t'') = T $ M.unionWith merge t' t''

fromString :: String -> T 
fromString [] = E
fromString (c:cs) = T (M.singleton c $ fromString cs)

insertT :: String -> T -> T
insertT = merge . fromString

count :: T -> Int
count E = 1
count (T t) = (1+) . sum . map count . M.elems $ t


