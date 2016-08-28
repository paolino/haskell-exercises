import qualified Data.Array as S
import Control.Applicative
import Control.Arrow
import Data.Monoid


main = do
    n <- readLn
    let f _ 0 = return ()
        f xt@(xs:_) n = do
        (c,rs) <- (head &&& tail) <$> words <$> getLine
        xt' <- case read c of
            1 -> return $ xs <> S.fromList (head rs) : xt
            2 -> return $ S.take (S.length xs - (read $ head rs)) xs : xt
            3 -> putStrLn (return  (S.! xs (read (head rs) - 1))) >> return xt
            4 -> return $ tail xt
        f xt' (n -1)
    f [S.fromList ""] n
