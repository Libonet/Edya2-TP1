data TTree k v = Node k (Maybe v ) (TTree k v ) (TTree k v ) (TTree k v )
                  | Leaf k v
                  | E deriving Show

-- devuelve el valor asociado a una clave
search :: Ord k => [k] -> TTree k v -> Maybe v
search _ E = Nothing
search [] _ = Nothing
search letra@[c] (Leaf k val) | c==k = Just val
                              | otherwise = Nothing
search letra@[c] (Node k mVal l m r) | c==k = mVal
                                     | c<k  = search letra l
                                     | otherwise = search letra r 
search str@(c:cs) (Leaf k val) = Nothing
search str@(c:cs) (Node k mVal l m r) | c<k  = search str l
                                      | c>k  = search str r
                                      | otherwise = search cs m

-- insert agrega un par (clave, valor) a un arbol. 
-- Si la clave ya esta en el arbol, actualiza su valor.
insert :: Ord k => [k] -> v -> TTree k v -> TTree k v
insert [] x tree = tree
insert [c] x E = Leaf c x
insert (c:cs) x E = Node c Nothing E (insert cs x E) E
insert [c] x (Leaf k val) | c==k = Leaf k x
                          | c<k  = Node k (Just val) (Leaf c x) E E
                          | otherwise = Node k (Just val) E E (Leaf c x)
insert (c:cs) x (Leaf k val) | c==k = Node k (Just val) E (insert cs x E) E
                             | c<k  = Node k (Just val) (Node c Nothing E (insert cs x E) E) E E
                             | otherwise = Node k (Just val) E E (Node c Nothing E (insert cs x E) E)
insert [c] x (Node k mval l m r) | c==k      = Node k (Just x) l m r
                                 | c<k       = Node k mval (insert [c] x l) m r
                                 | otherwise = Node k mval l m (insert [c] x r)
insert (c:cs) x (Node k mval l m r) | c==k = Node k mval l (insert cs x m) r
                                    | c<k  = Node k mval (insert (c:cs) x l) m r
                                    | otherwise = Node k mval l m (insert (c:cs) x r)

-- dado un arbol devuelve una lista ordenada con las claves del mismo
keys :: TTree k v -> [[k]]
keys E = []
keys (Leaf k v) = [[k]]
keys (Node k Nothing l m r) = map (k :) (keys m) ++ keys l ++ keys r
keys (Node k val l m r) = [k] : map (k :) (keys m) ++ keys l ++ keys r

-- elimina una clave y el valor asociada a ´esta en un ´arbol
delete :: Ord k => [k] -> TTree k v -> TTree k v
delete [] arbol = arbol
delete _ E = E
delete [x] hoja@(Leaf clave valor) = if x == clave then E else hoja
delete xs hoja@(Leaf _ _) = hoja
delete elem@[x] (Node clave valor l c r) | x > clave = Node clave valor l c (delete elem r)
                                         | x < clave = Node clave valor (delete elem l) c r
                                         | otherwise = borrar clave l c r
                                         where
                                          borrar _ E E E = E
                                          borrar clave l c r = Node clave Nothing l c r
delete lista@(x:xs) (Node clave valor l c r) | x > clave = Node clave valor l c (delete lista r)
                                             | x < clave = Node clave valor (delete lista l) c r
                                             | otherwise = Node clave valor l (delete xs c) r

t = Node 'r' Nothing E (Node 'e' (Just 16) (Node 'a' Nothing E (Leaf 's' 1) E)
                                          (Node 'o' (Just 2) (Leaf 'd' 9)
                                                              E
                                                              (Leaf 's' 4))
                                          E)
                      (Node 's' Nothing E (Node 'i' (Just 4) (Leaf 'e' 8)
                                                              (Leaf 'n' 7)
                                                              E)
                                          E)


