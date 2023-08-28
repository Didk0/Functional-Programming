import FileSystem

simpleTree :: FileTree
simpleTree = Directory "/" 
                [File "file1" "This is file1",
                File "file2" "This is file2",
                File "file3" "",
                Directory "dir1" 
                        [File "file1" "This is file3",
                        Directory "dir3" 
                                [File "file4" "This is file4"],
                        Directory "dir4" []],
                Directory "dir2" 
                        [File "file4" "This is file4"]]

bigTree :: FileTree
bigTree = Directory "/" 
                [Directory "ip" 
                        [Directory "homeworks" 
                                [File "hw1" "simple clock implementation.",
                                File "hw2" "working with dates.",
                                File "hw3" "sorting rows of a matrix."],
                        Directory "exercises" 
                                [File "exc1" "this is introducing to the syntax in C++"]],
                Directory "oop" 
                        [Directory "homeworks" 
                                [File "hw1" "big four",
                                File "hw2" "dynamic array"],
                        Directory "project" 
                                [Directory "source" 
                                        [File "s" "this is source file",
                                        File "et" "this is electonic table project"]]],
                Directory "dsa" 
                        [File "content" "here are the recourses from dsa",
                        Directory "homeworks" 
                                [File "hw1" "store with client lists",
                                File "hw2" "tree hierarchy with workers",
                                File "hw3" "hash table for file comparison"],
                        Directory "project" 
                                [File "db" "this is database project",
                                Directory "implementation" 
                                        [File "impl" "this is the database"]]],
                Directory "fp" 
                        [Directory "homeworks" 
                                [File "hw1" "some scheme tasks",
                                File "hw2" "and some haskell tasks"],
                        Directory "project" 
                                [File "file system" "this is the project"]]]

startingPath :: Path
startingPath = ["/"]

readInput :: IO String
readInput = do
        line <- getLine
        if line == "." 
        then return ""
        else do
            rest <- readInput
            if null rest then return line else return (line ++ "\n" ++ rest)

printOutput :: String -> IO ()
printOutput str = if str == "" then putStr str else putStrLn str

tokenize :: String -> Arguments 
tokenize str = helper str [] [] where
        helper str currToken result
                | null str = if null currToken then result else result++[currToken]
                | head str == ' ' = if null currToken then helper (tail str) [] result else helper (tail str) [] (result++[currToken])
                | otherwise = helper (tail str) (currToken++[head str]) result

main :: IO ()
main = do    
    helper startingPath simpleTree where
    helper :: Path -> FileTree -> IO ()
    helper path tree = do
        putStr "$ "
        command <- getLine
        case tokenize command of
            ["pwd"] -> putStrLn (pwd path)
            ("cd" : tokens) -> case tokens of
                [] -> helper (cd path tree "") tree
                [token] -> helper (cd path tree token) tree
                _ -> pure()
            ("ls" : tokens) -> case tokens of
                [] -> printOutput (ls [] path tree)
                [token] -> printOutput (ls token path tree)
                _ -> pure()
            ("cat" : tokens) -> case tokens of
                [] -> putStrLn "Invalid command!"
                tokens
                    | length [tok | tok <- tokens, tok == ">"] > 1 -> putStrLn "Invalid command!"
                    | length tokens == length (takeWhile (/= ">") tokens) -> printOutput (concatFiles tokens path tree)
                    | not (null (takeWhile (/= ">") tokens)) && last (init tokens) == ">"
                         -> helper path (cat (takeWhile (/= ">") tokens) (last tokens) path tree)
                    | length tokens == 2 -> do 
                        result <- readInput 
                        helper path (cat' result (last tokens) path tree) 
                    | otherwise -> putStrLn "Invalid command!"
            ("rm" : tokens) -> case tokens of
                [] -> putStrLn "Invalid command!"
                tokens -> helper path (rm tokens path tree)
            ("touch" : tokens) -> case tokens of
                [] -> putStrLn "Invalid command!"
                tokens -> helper path (touch path (last tokens) tree)
            ("mkdir" : tokens) -> case tokens of
                [] -> putStrLn "Invalid command!"
                tokens -> helper path (mkdir path (last tokens) tree)
            _ -> putStrLn "Unknown command!"
        helper path tree