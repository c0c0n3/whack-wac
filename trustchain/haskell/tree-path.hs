--
-- Finding a path in a tree.
-- Run the example with the command below
--
--   $ ghc --run tree-path.hs
--


-- Canonical multi-way tree algebraic data type.
data Tree a = Node a [Tree a]

-- Find the path (if any) from the tree root to the node satisfying
-- the given predicate.
path :: (a -> Bool) -> Tree a -> [a]
path p = collect []
  where
  collect xs (Node a ts) | p a       = a:xs
                         | otherwise = concatMap (collect (a:xs)) ts


-- Example tree.
t = Node 0
      [ Node 1
          [ Node 11 []
          , Node 12 []
          ]
      , Node 2
          [ Node 21 []
          , Node 22 []
          ]
      ]
-- Example evaluation of path.
r = path (==22) t

main :: IO ()
main = do
  print r  -- prints [22,2,0]
