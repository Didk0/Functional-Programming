module FileSystem where

-- module for the logic of all functions for the file system
------------------------------------------------------------------------------------------

data FileTree = EmptyTree
                | File String String
                | Directory String [FileTree]
                deriving (Eq, Show)

type Path = [String]
type Arguments = [String]

------------------------------------------------------------------------------------------

-- some helper functions

-- converts a given path from String to Path by removing "/" between the elements
getStrAsPath :: String -> Path
getStrAsPath newPath = helper newPath "" [] where
        helper :: String -> String -> Path -> Path
        helper path currWord res
                | null path = res++[currWord]
                | head path == '/' && length path == length newPath = helper (tail path) "/" res
                | head path == '/' = helper (tail path) "" (res++[currWord])
                | otherwise = helper (tail path) (currWord++[head path]) res

-- searches for a File and returns it if found and EmptyTree - otherwise
findFile :: Path -> FileTree -> FileTree
findFile [] _ = EmptyTree
findFile _ EmptyTree = EmptyTree
findFile [p] file@(File name _) = if name == p then file else EmptyTree
findFile path (File _ _) = EmptyTree
findFile path dir@(Directory name successors) 
        | name == head path = if null [c | c <- map (findFile $ tail path) successors, c /= EmptyTree] 
                              then EmptyTree
                              else head [c | c <- map (findFile $ tail path) successors, c /= EmptyTree]
        | otherwise = EmptyTree

-- gives the name of a File
getFileName :: FileTree -> String
getFileName (File f val) = f
getFileName _ = ""

-- gives the value of a File
getFileValue :: FileTree -> String
getFileValue (File f val) = val
getFileValue _ = ""

getDirName :: FileTree -> String
getDirName (Directory d successors) = d
getDirName _ = ""

-- check if a given Path is a valid path to a File
isValidPathToFile :: Path -> FileTree -> Bool
isValidPathToFile path tree = findFile path tree /= EmptyTree 

-- searches for a Directory and returns it if found and EmptyTree - otherwise
findDirectory :: Path -> FileTree -> FileTree
findDirectory [] _ = EmptyTree
findDirectory _ EmptyTree = EmptyTree
findDirectory _ (File f val) = EmptyTree
findDirectory [p] (Directory name successors) = if p == name then Directory name successors else EmptyTree
findDirectory path (Directory d successors) 
                | head path == d = if null [child | child <- map (findDirectory $ tail path) successors, child /= EmptyTree] 
                                   then EmptyTree
                                   else head [child | child <- map (findDirectory $ tail path) successors, child /= EmptyTree]
                | otherwise = EmptyTree

-- check if a given Path is a valid path to a Directory
isValidPathToDir :: Path -> FileTree -> Bool
isValidPathToDir path tree = findDirectory path tree /= EmptyTree

-- by given relative path as String and the current Path returns the full path as Path
fullPath :: String -> Path -> Path
fullPath arg path = if length path == 1 
                        then getStrAsPath $ getPathAsStr path ++ "/" ++ arg 
                        else getStrAsPath $ getPathAsStr ("/" : path) ++ "/" ++ arg

------------------------------------------------------------------------------------------

-- logic for pwd

-- converts the current path from Path to String by putting "/" between the elements
getPathAsStr :: Path -> String
getPathAsStr [] = "/"
getPathAsStr [x] = if x == "/" then "/" else x
getPathAsStr (x:xs) 
        | x == "/" = x ++ getPathAsStr xs
        | otherwise = x ++ "/" ++ getPathAsStr xs

-- gives the current path
pwd :: Path -> String
pwd = getPathAsStr

------------------------------------------------------------------------------------------

-- logic for cd

-- removes all pairs of the type dir/..
removeParentDirs :: String -> String
removeParentDirs argument = getPathAsStr (filterParentDirs (getStrAsPath argument) []) where
        filterParentDirs oldPath res
                | null oldPath = res
                | null (tail oldPath) && head oldPath /= ".." = res++[head oldPath]
                | head oldPath /= ".." && head (tail oldPath) == ".." = filterParentDirs (tail (tail oldPath)) res
                | otherwise = filterParentDirs (tail oldPath) (res++[head oldPath])  

-- check if there are any pairs of the type dir/.. left
existParentDirs :: String -> Bool
existParentDirs arg = helper (getStrAsPath arg) where
        helper args
                | null args = False
                | null (tail args) = False
                | head args /= ".." && head (tail args) == ".." = True
                | otherwise = helper (tail args) 

-- changes the current path to a new one
cd :: Path -> FileTree -> String -> Path
cd path tree arg
        | arg == "" = ["/"]
        | null path = ["/"]
        | existParentDirs arg = cd path tree (removeParentDirs arg)
        | length arg >= 3 && head arg == '.' && arg !! 1 == '.' && arg !! 2 == '/' = cd (init path) tree (drop 3 arg)
        | length arg == 2 && head arg == '.' && arg !! 1 == '.' = if length path > 1 then init path else ["/"]
        | head arg == '/' = if isValidPathToDir (getStrAsPath ("/" ++ arg)) tree then getStrAsPath ("/" ++ arg) else path
        | otherwise = if isValidPathToDir (fullPath arg path) tree 
                      then fullPath arg path 
                      else path
 
------------------------------------------------------------------------------------------

-- logic for ls

-- sets commas between elements of [String]
setCommasToStr :: [String] -> String
setCommasToStr [] = ""
setCommasToStr [x] = x
setCommasToStr (x:xs) = x ++ ", " ++ setCommasToStr xs

-- gives the successors of a Directory
successors :: FileTree -> [String]
successors EmptyTree = []
successors (File _ _) = []
successors (Directory _ children) = [getTypeName c | c <- children] where
                getTypeName EmptyTree = ""
                getTypeName (File f _) = f
                getTypeName (Directory d _) = d

-- gives the content of the current directory
ls :: String -> Path -> FileTree -> String
ls "" path tree = if isValidPathToDir path tree then setCommasToStr $ successors $ findDirectory path tree else ""
ls arg path tree
        | head arg == '/' = if isValidPathToDir (getStrAsPath ("/" ++ arg)) tree then setCommasToStr $ successors $ findDirectory (getStrAsPath ("/" ++ arg)) tree else []
        | otherwise = if isValidPathToDir (fullPath arg path) tree
                      then setCommasToStr $ successors $ findDirectory (fullPath arg path) tree else []
                              

------------------------------------------------------------------------------------------

-- logic for rm

-- removes a file by a given Path, name of the file and FileTree
removeFile :: Path -> String -> FileTree -> FileTree
removeFile _ _ EmptyTree = EmptyTree
removeFile [] _ file@(File f val) = file
removeFile [] _ dir@(Directory d successors) = dir
removeFile [p] toRemove file@(File f val) = if f == toRemove then EmptyTree else file
removeFile path _ file@(File f val) = file
removeFile path toRemove (Directory dir successors) = Directory dir (filter (/= EmptyTree) (map (removeFile (tail path) toRemove) successors))

-- removes all files given as Arguments from the FileTree
rm :: Arguments -> Path -> FileTree -> FileTree
rm [] path tree = tree
rm (f:fs) path tree
        | head f == '/' = rm fs path (removeFile (getStrAsPath ("/" ++ f)) fileToRemove tree)
        | otherwise = rm fs path (removeFile (fullPath f path) fileToRemove tree) where
                fileToRemove = reverse $ takeWhile (/= '/') (reverse f)

------------------------------------------------------------------------------------------

-- logic for cat

-- adds a file to the FileTree by given path, name and value or does nothing if path is incorect
addFile :: Path -> String -> String -> FileTree -> FileTree
addFile [] _ _ _ = EmptyTree
addFile _ _ _ EmptyTree = EmptyTree
addFile path name value (File f val) = File f val
addFile [p] name value (Directory d successors) = if d == p then Directory d (successors++[File name value]) else Directory d successors
addFile path name value (Directory d successors) = Directory d (map (addFile (tail path) name value) successors)

-- replaces a File by given path with new name and value and does nothing if the path is not correct
replaceFile :: Path -> String -> String -> FileTree -> FileTree
replaceFile [] _ _ _ = EmptyTree
replaceFile _ _ _ EmptyTree = EmptyTree
replaceFile [p] name value file@(File f val) = if p == f then File name value else file
replaceFile [p] name value dir@(Directory f successors) = dir
replaceFile path name value file@(File f _) = file
replaceFile path name value (Directory d successors) = Directory d (map (replaceFile (tail path) name value) successors)

-- concatenates the value of Arguments and returns the new value
concatFiles :: Arguments -> Path -> FileTree -> String
concatFiles [] path tree = ""
concatFiles (f:fs) path tree 
        | head f == '/' = if null fs || cond (head fs)
                            then content f ++ concatFiles fs path tree
                            else content f ++ "\n" ++ concatFiles fs path tree
        | otherwise = if null fs || cond (head fs)
                        then content f ++ concatFiles fs path tree 
                        else content f ++ "\n" ++ concatFiles fs path tree where
                cond file = if head file == '/' then null (getFileValue (findFile (getStrAsPath ("/" ++ file)) tree))
                                                else null (getFileValue (findFile (fullPath file path) tree))
                content file = if head file == '/' then getFileValue (findFile (getStrAsPath ("/" ++ file)) tree)
                                              else getFileValue (findFile (fullPath file path) tree)

-- concatenates the values of Arguments as a value result and adds it as a new File to the FileTree
-- in the given result, which is path as string, or replaces it's old value if the File already exists
cat :: Arguments -> String -> Path -> FileTree -> FileTree
cat files result path tree
        | head result == '/' && fileResult result `elem` successors (findDirectory (init (getStrAsPath ("/" ++ result))) tree) 
                = replaceFile (getStrAsPath ("/" ++ result)) (fileResult result) (concatFiles (clearDirs files) path tree) tree

        | head result /= '/' && fileResult result `elem` successors (findDirectory (init (fullPath result path)) tree)
                = replaceFile (fullPath result path) (fileResult result) (concatFiles (clearDirs files) path tree) tree

        | head result == '/' && fileResult result `notElem` successors (findDirectory (init (getStrAsPath ("/" ++ result))) tree)
                = addFile (init (getStrAsPath ("/" ++ result))) (fileResult result) (concatFiles (clearDirs files) path tree) tree

        | head result /= '/' && fileResult result `notElem` successors (findDirectory (init (fullPath result path)) tree)
                = addFile (init (fullPath result path)) (fileResult result) (concatFiles (clearDirs files) path tree) tree

        | otherwise = tree where
                fileResult f = reverse $ takeWhile (/= '/') (reverse f)
                isFPath file = if head file == '/' then isValidPathToFile (getStrAsPath ("/" ++ file)) tree
                                                   else isValidPathToFile (fullPath file path) tree
                clearDirs files = filter isFPath files

-- same command as cat but recieves a concatenated value
cat' :: String -> String -> Path -> FileTree -> FileTree
cat' resValue result path tree 
        | head result == '/' && fileResult result `elem` successors (findDirectory (init (getStrAsPath ("/" ++ result))) tree) 
                = replaceFile (getStrAsPath ("/" ++ result)) (fileResult result) resValue tree

        | head result /= '/' && fileResult result `elem` successors (findDirectory (init (fullPath result path)) tree)
                = replaceFile (fullPath result path) (fileResult result) resValue tree

        | head result == '/' && fileResult result `notElem` successors (findDirectory (init (getStrAsPath ("/" ++ result))) tree)
                = addFile (init (getStrAsPath ("/" ++ result))) (fileResult result) resValue tree

        | head result /= '/' && fileResult result `notElem` successors (findDirectory (init (fullPath result path)) tree)
                = addFile (fullPath result path) (fileResult result) resValue tree

        | otherwise = tree where
                fileResult :: String -> String
                fileResult f = reverse $ takeWhile (/= '/') (reverse f)

------------------------------------------------------------------------------------------

-- logic for touch

-- makes empty file with given name, Path and FileTree
makeEmptyFile :: Path -> String -> String -> FileTree -> FileTree
makeEmptyFile [] _ _ _ = EmptyTree
makeEmptyFile _ _ _ EmptyTree = EmptyTree
makeEmptyFile path dir name (File f val) = File f val
makeEmptyFile [p] dir name (Directory d children) = if d == dir && name `notElem` [getFileName c | c <- children]
                                                         then Directory d (children++[File name ""])
                                                         else Directory d children
makeEmptyFile path dir name (Directory d children) = Directory d (map (makeEmptyFile (tail path) dir name) children)

-- makes empty file by given path to directory
touch :: Path -> String -> FileTree -> FileTree
touch path newFile tree
                | head newFile == '/' = makeEmptyFile (init (getStrAsPath ("/" ++ newFile))) (last (init (getStrAsPath ("/" ++ newFile)))) resFile tree
                | head newFile /= '/' = makeEmptyFile (init (fullPath newFile path)) (last (init (fullPath newFile path))) resFile tree
                | otherwise = tree where
                        resFile = reverse $ takeWhile (/= '/') (reverse newFile)

------------------------------------------------------------------------------------------

-- logic for mkdir

-- makes empty directory with given name, Path and FileTree
makeEmptyDir :: Path -> String -> String -> FileTree -> FileTree
makeEmptyDir [] _ _ _ = EmptyTree
makeEmptyDir _ _ _ EmptyTree = EmptyTree
makeEmptyDir path dir name (File f val) = File f val
makeEmptyDir [p] dir name (Directory d children) = if d == dir && name `notElem` [getDirName c | c <- children]
                                                        then Directory d (children++[Directory name []]) 
                                                        else Directory d children
makeEmptyDir path dir name (Directory d children) = Directory d (map (makeEmptyDir (tail path) dir name) children)

-- makes empty directory by given path to directory
mkdir :: Path -> String -> FileTree -> FileTree
mkdir path newDir tree
        | head newDir == '/' = makeEmptyDir (init (getStrAsPath ("/" ++ newDir))) (last (init (getStrAsPath ("/" ++ newDir)))) resDir tree
        | head newDir /= '/' =  makeEmptyDir (init (fullPath newDir path)) (last (init (fullPath newDir path))) resDir tree
        | otherwise = tree where
                resDir = reverse $ takeWhile (/= '/') (reverse newDir)

------------------------------------------------------------------------------------------