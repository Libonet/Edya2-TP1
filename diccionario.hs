data TTree k v = Node k (Maybe v ) (TTree k v ) (TTree k v ) (TTree k v )
                  | Leaf k v
                  | E

search :: Ord k => [k] -> TTree k v -> Maybe v
search _ E = Nothing
search [] (Leaf x val) = Just val
search [] (Node x mVal l m r) = mVal
search (c:cs) (Leaf x val) = Nothing
search (c:cs) (Node x mVal l m r) | c<x  = search cs l
                                  | c>x  = search cs r
                                  | c==x = search cs m
                                  | otherwise = Nothing

t = Node 'r' Nothing E (Node 'e' (Just 16) (Node 'a' Nothing E (Leaf 's' 1) E)
                                          (Node 'o' (Just 2) (Leaf 'd' 9)
                                                              E
                                                              (Leaf 's' 4))
                                          E)
                      (Node 's' Nothing E (Node 'i' (Just 4) (Leaf 'e' 8)
                                                              (Leaf 'n' 7)
                                                              E)
                                          E)


