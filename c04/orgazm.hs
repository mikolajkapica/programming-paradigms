data LBt a = LNode a (LBt a) (LBt a) | Empty deriving (Show, Eq)

ltree :: Num t => t -> LBt t
ltree n = LNode n (ltree (2*n)) (ltree (2*n+1))

ltake 0 _ = Empty
ltake n (LNode a left right) = LNode a (ltake (n-1) left) (ltake (n-1) right)

lprint :: Show a => LBt a -> IO ()
lprint (LNode a left right) = do
  putStr $ show a ++ " "
  lprint left
  lprint right
lprint Empty = return ()