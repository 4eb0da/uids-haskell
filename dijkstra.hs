infinity = 100500 :: Float

initialDistance :: Int -> Int -> Int -> [(Int, Float, Bool)]
initialDistance index n s
	| n == 0 = []
	| s == 0 = (index, 0, False) : initialDistance (index + 1) (n - 1) (s - 1)
	| otherwise = (index, infinity, False) : initialDistance (index + 1) (n - 1) (s - 1)

nodeIndex :: (Int, Float, Bool) -> Int
nodeIndex (index, _, _) = index

nodeDistance :: (Int, Float, Bool) -> Float
nodeDistance (_, dist, _) = dist

hasNodeMark :: (Int, Float, Bool) -> Bool
hasNodeMark (_, _, mark) = mark

edgeFirstVertex :: (Int, Int, Float) -> Int
edgeFirstVertex (vertex0, _, _) = vertex0

edgeSecondVertex :: (Int, Int, Float) -> Int
edgeSecondVertex (_, vertex1, _) = vertex1

edgeLength :: (Int, Int, Float) -> Float
edgeLength (_, _, len) = len

unmarkedNodeWithMinimalDistance :: [(Int, Float, Bool)] -> (Int, Float, Bool)
unmarkedNodeWithMinimalDistance [x] = x
unmarkedNodeWithMinimalDistance (x:y:xs)
	| (nodeDistance x) < nodeDistance (unmarkedNodeWithMinimalDistance (y:xs)) = x
	| otherwise = (unmarkedNodeWithMinimalDistance (y:xs))

nearestNode :: [(Int, Float, Bool)] -> (Int, Float, Bool)
nearestNode vertices = unmarkedNodeWithMinimalDistance [node | node <- vertices, not (hasNodeMark node)]

markNode :: (Int, Float, Bool) -> (Int, Float, Bool)
markNode (index, dist, _) = (index, dist, True)

updateNodeDistance :: (Int, Float, Bool) -> Float -> (Int, Float, Bool)
updateNodeDistance node dist = (nodeIndex node, dist, hasNodeMark node)

minimalNodeDistance :: (Int, Float, Bool) -> (Int, Float, Bool) -> [(Int, Int, Float)] -> Float
minimalNodeDistance node currentNode [] = nodeDistance node
minimalNodeDistance node currentNode (x:xs) = min (nodeDistance currentNode + edgeLength x) (minimalNodeDistance node currentNode xs)

updateNode :: (Int, Float, Bool) -> (Int, Float, Bool) -> [(Int, Int, Float)] -> (Int, Float, Bool)
updateNode node currentNode edges = updateNodeDistance node (minimalNodeDistance node currentNode [edge | edge <- edges, edgeFirstVertex edge == nodeIndex currentNode, edgeSecondVertex edge == nodeIndex node])

updateVertices :: (Int, Float, Bool) -> [(Int, Float, Bool)] -> [(Int, Int, Float)] -> [(Int, Float, Bool)]
updateVertices currentNode vertices edges = [(updateNode node currentNode edges) | node <- vertices, nodeIndex node < nodeIndex currentNode] ++
											[currentNode] ++
											[(updateNode node currentNode edges) | node <- vertices, nodeIndex node > nodeIndex currentNode]

processStep :: [(Int, Int, Float)] -> [(Int, Float, Bool)] -> [(Int, Float, Bool)]
processStep edges vertices = updateVertices (markNode (nearestNode vertices)) vertices edges

distance :: [(Int, Int, Float)] -> Int -> Int -> [(Int, Float, Bool)] -> Float
distance edges end elapsed vertices
	| elapsed == 0 = nodeDistance (vertices !! end)
	| otherwise = distance edges end (elapsed - 1) (processStep edges vertices)

dijkstra :: Int -> [(Int, Int, Float)] -> Int -> Int -> Float
dijkstra n edges start end = distance edges end n (initialDistance 0 n start)