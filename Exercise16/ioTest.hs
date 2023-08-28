import Data.Char (isPunctuation)

main :: IO ()
main = do
    line <- getLine

    if line == ":q"
      then return ()
      else do
          let punctuation = filter isPunctuation line
              exclamation = [p | p <- punctuation, p == '!']
          putStrLn punctuation
          putStrLn exclamation
          main

-- class Monad m where
--     return :: a -> m a
--     (>>=) :: m a -> (a -> m b) -> m b

-- instance Monad (Maybe a) where
--     return a = Just a
--     (>>=) Nothing _ = Nothing
--     (>>=) (Just x) f = Just (f x)

-- instance Monad [a] where
--     return a = [a]
--     (>>=) [] _ = []
--     (>>=) (x:xs) f = f x ++ (xs >== f)