-- main::IO()
-- main = print 0

import Data.List (nub, sort,sortBy, maximumBy)
import Data.Char (isLower, toUpper, isSpace, isPunctuation, isNumber)

-- zad3 ТК

groceries :: [(String, String, Double)]
groceries = [
    ("Kaufland", "eggs", 2.69),
    ("Billa", "eggs", 2.79),
    ("FF", "eggs", 2.99),
    ("345", "eggs", 5.00),
    ("Kaufland", "banana", 2.29),
    ("Billa", "banana", 2.59),
    ("FF", "banana", 2.19),
    ("Kaufland", "broccoli", 4.59),
    ("Billa", "broccoli", 5.20),
    ("FF", "broccoli", 4.99),
    ("Kaufland", "cheese", 17.69),
    ("Billa", "cheese", 18.79),
    ("FF", "cheese", 19.99),
    ("Lidl", "skyr", 2.17)
    ]

getStore :: (String, String, Double) -> String
getStore(store, _, _) = store

getCategory :: (String, String, Double) -> String
getCategory(_, category, _) = category

getPrice :: (String, String, Double) -> Double
getPrice(_, _, price) = price

-- maxTotalPrice :: [(String, String, Double)] -> [(String, Double, String)]
-- maxTotalPrice shoppingList = uniq [(category, getTotalPrice category, getHighestPriceStore category) | (_, category, _) <- shoppingList] where
--     getTotalPrice category = sum [price | (_, c, price) <- shoppingList, c == category]
--     getHighestPriceStore category = getStore $ foldr1 (\currItem@(_, _, currPrice) maxItem@(_, _, maxPrice) -> if currPrice > maxPrice then currItem else maxItem)
--         [item | item <- shoppingList, category == getCategory item]
--     uniq [] = []
--     uniq (x:xs) = if x `elem` xs then uniq xs else x : uniq xs

comparePrices :: (String, String, Double) -> (String, String, Double) -> Ordering
comparePrices item1 item2 
    | getPrice item1 > getPrice item2 = GT
    | getPrice item1 < getPrice item2 = LT
    | otherwise = EQ

maxTotalPrice :: [(String, String, Double)] -> [(String, Double, String)]
maxTotalPrice shoppingList = nub [(category, getTotalPrice category, getHighestPriceStore category) | (_, category, _) <- shoppingList] where
    getTotalPrice category = sum [price | (_, c, price) <- shoppingList, c == category]
    getHighestPriceStore category = getStore $ maximumBy comparePrices [item | item <- shoppingList, category == getCategory item]

contains :: Char -> String -> String
contains _ [] = "nope"
contains c str@(x:xs) = if c == x then "we found it in " ++ str else contains c xs 