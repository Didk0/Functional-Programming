--zad 1
isBijection :: (Int -> Int) -> [Int] -> Bool
isBijection f interval = isInection f && isSurjection f where
    isInection f = all cond1 interval where
        cond1 x = any (\y -> x == y && f x == f y) interval
    isSurjection f = all cond2 interval where
        cond2 x = any (\y -> f y == x) interval

isNPerm :: Int -> (Int -> Int) -> Bool
isNPerm n f = isBijection f [0..n-1]

-- maxCycle :: Int -> (Int -> Int) -> [Int]
-- maxCycle n f = helper [0..n-1] f []  where
--     helper interval f res
--         | null (tail interval) = res
--         | f (head interval) == head (tail interval) = helper (tail interval) f (head interval:res)
--         | otherwise = helper (tail interval) f res

--zad 2
movingAverage :: [Double] -> Int -> [Double]
movingAverage stream n 
    | n < 2 = error "Invalid input!"
    | otherwise = current : movingAverage (tail stream) n where
        current = sum (take n stream) / fromIntegral n

allAverages :: [Double] -> [[Double]]
allAverages stream = [generateStream stream ind | ind <- [2..]] where 
    generateStream stream i = movingAverage stream i

--zad 3

type Inventory = [(String, [String])]

inv :: Inventory
inv = [ ("docs", ["ids", "invoices"]), ("ids", ["passport"]),  ("invoices", []), ("memes", []),
        ("family", ["new year", "birthday"]), ("funny", ["memes"]), ("pics", ["family", "funny"]) ]

takeObjectsOfItem :: (String, [String]) -> Inventory -> [String]
takeObjectsOfItem item inventory = helper itemList inventory [] where
    itemList = snd item
    helper :: [String] -> Inventory -> [String] -> [String]
    helper lst inv res
        | null lst = res
        | head lst `notElem` [name | (name, _) <- inventory] = helper (tail lst) inv (head lst:res)
        | otherwise = helper (tail lst) inv res        

allObjects :: Inventory -> [String]
allObjects inventory = helper inventory [] where 
    helper :: Inventory -> [String] -> [String]
    helper inv res
        | null inv = res
        | otherwise = helper (tail inv) (res++takeObjectsOfItem (head inv) inventory)

cleanUp :: Inventory -> Inventory
cleanUp inventory  
        | not $ hasEmptyBoxes inventory = inventory
        | otherwise = cleanUp (removeOnce inventory)  where
            hasEmptyBoxes i = not $ null [item | item <- i, null $ snd item]
            removeOnce inv = filter (`notElem` removed) [clearItem elem | elem <- inv] where 
                clearItem i = (fst i, [x | x <- snd i, not $ mustBeRemoved x]) where
                    mustBeRemoved el = not $ null [s | s <- inv, fst s == el && null (snd s)]
                removed = [x | x <- inv, null $ snd x]
    