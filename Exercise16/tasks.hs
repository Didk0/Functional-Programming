type Vertex = Char
type Neighbours = [Vertex]
type Graph = [(Vertex, Neighbours)]

graph :: Graph
graph = [
    ('a', ['b', 'e']),
    ('b', ['c', 'd']),
    ('c', ['f']),
    ('d', ['c']),
    ('e', []),
    ('f', [])
    ]

children :: Vertex -> Graph -> Neighbours
children _ [] = error "empty graph"
children v (x:xs) = if v == fst x then snd x else children v xs

data Stack a = EmptyStack | Stack {top :: a, rest :: Stack a} deriving (Eq)
instance Show a => Show (Stack a) where
    show stack = "Stack(" ++ showHelper stack [] ++ ")" where
        showHelper EmptyStack result = show result
        showHelper stack result = showHelper (rest stack) (result ++ [top stack])

isEmpty :: Stack a -> Bool
isEmpty EmptyStack = True
isEmpty _ = False

push :: Stack a -> a -> Stack a
push EmptyStack value = Stack value EmptyStack
push stack value = Stack value stack

pushMultiple :: Stack a -> [a] -> Stack a
-- pushMultiple stack [] = stack
-- pushMultiple stack (x:xs) = pushMultiple (push stack x) xs
pushMultiple = foldl push

dfs :: Vertex -> Graph -> [Vertex]
dfs _ [] = error "empty graph"
dfs v g@(x:xs) = dfsHelper (Stack v EmptyStack) [] where
    dfsHelper EmptyStack _ = []
    dfsHelper (Stack t rest) visited
        | t `elem` visited = dfsHelper rest visited
        | otherwise = t : dfsHelper (pushMultiple rest (children t g)) (t : visited)