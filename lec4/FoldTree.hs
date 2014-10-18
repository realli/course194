module FoldTree where

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Eq, Show)

-- fold a list into balance tree
foldTree :: [a] -> Tree a
foldTree = foldl insert Leaf

-- insert a node into Tree
insert :: Tree a -> a -> Tree a
insert Leaf ele = Node 1 Leaf ele Leaf
insert (Node height left value right) ele
    | getDepth left <= getDepth right = let newLeft = insert left ele
                                            leftHeight = getDepth newLeft
                                        in Node (max height (leftHeight + 1)) newLeft value right
    | otherwise = let newRight = insert right ele
                      rightHeight = getDepth newRight
                  in Node (max height (rightHeight + 1)) left value newRight    
  where getDepth Leaf = 0
        getDepth (Node h _ _ _) = h

