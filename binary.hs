
data BinaryTree  a = Empty 
  | Node (BinaryTree a) a (BinaryTree a) 
  | Leaf a
  deriving (Show)



insertTree :: BinaryTree Int -> Int -> BinaryTree Int
insertTree Empty value = Node(Empty) value (Empty) 
insertTree (Node left root right) value | value < root = Node (insertTree left value) root (right)
                                        | value > root = Node left root (insertTree right value)   
                                        | otherwise = Node left root right
                                        

list2Tree :: [Int]->BinaryTree Int
list2Tree lst = foldl (insertTree) Empty lst

tree2List :: BinaryTree Int -> [Int]
tree2List (Node Empty root Empty) = [root]
tree2List (Node left root Empty) = tree2List(left) ++ [root]
tree2List (Node Empty root right) =  [root] ++ tree2List(right)
tree2List (Node left root right) = tree2List(left) ++ [root] ++ tree2List(right)

inTree :: Int->BinaryTree Int->Bool
inTree value Empty = False
inTree value (Node left root right) | value == root = True
                                    | value < root = inTree value left
                                    | value > root = inTree value right
                                     
mergeTree :: BinaryTree Int -> BinaryTree Int -> BinaryTree Int
mergeTree tree1 tree2 = list2Tree( tree2List(tree1)++tree2List(tree2) )

