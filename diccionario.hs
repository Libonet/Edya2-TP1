{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

data TTree k v = Node k (Maybe v ) (TTree k v ) (TTree k v ) (TTree k v )
                  | Leaf k v
                  | E deriving Show


-- devuelve el valor asociado a una clave
search :: Ord k => [k] -> TTree k v -> Maybe v
search _ E = Nothing
search [] _ = Nothing
search str@(c:cs) (Leaf k val) = case cs of
                                [] -> if c==k then Just val else Nothing
                                _  -> Nothing
search str@(c:cs) (Node k mVal l m r) | c<k  = search str l
                                      | c>k  = search str r
                                      | otherwise = case cs of
                                            [] -> mVal
                                            _  -> search cs m


-- insert agrega un par (clave, valor) a un arbol. 
-- Si la clave ya esta en el arbol, actualiza su valor.
insert :: Ord k => [k] -> v -> TTree k v -> TTree k v
insert [] x tree = tree
insert [c] x E = Leaf c x
insert (c:cs) x E = Node c Nothing E (insert cs x E) E
insert list@(c:cs) x (Leaf k val) | c>k = Node k (Just val) E E insertado
                                  | c<k = Node k (Just val) insertado E E
                                  | otherwise = case cs of
                                              [] -> insertado
                                              _  -> Node k (Just val) E (insert cs x E) E
                                  where
                                    insertado = insert list x E
insert list@(c:cs) x (Node k mval l m r) | c>k = Node k mval l m (insert list x r)
                                         | c<k = Node k mval (insert list x l) m r
                                         | otherwise = case cs of
                                                  [] -> Node k (Just x) l m r
                                                  _  -> Node k mval l (insert cs x m) r


-- dado un arbol devuelve una lista ordenada con las claves del mismo
keys :: TTree k v -> [[k]]
keys E = []
keys (Leaf k v) = [[k]]
keys (Node k Nothing l m r) = keys l ++ map (k :) (keys m) ++ keys r
keys (Node k val l m r) = keys l ++ [[k]] ++ map (k :) (keys m) ++ keys r



-- elimina una clave y el valor asociada a ´esta en un ´arbol
delete :: Ord k => [k] -> TTree k v -> TTree k v
delete [] arbol = arbol
delete _ E = E
delete [x] hoja@(Leaf clave _) = if x == clave then E else hoja
delete xs hoja@(Leaf _ _) = hoja
delete lista@(x:xs) (Node clave valor l c r) | x > clave = borrar clave valor l c (delete lista r)
                                             | x < clave = borrar clave valor (delete lista l) c r
                                             | otherwise = case xs of
                                                      [] -> borrar clave Nothing l c r
                                                      _  -> borrar clave valor l (delete xs c) r
                                             where
                                              borrar _ Nothing E E E = E
                                              borrar clave (Just valor) E E E = Leaf clave valor
                                              borrar clave Nothing l E E = l
                                              borrar clave Nothing E E r = r
                                              borrar clave Nothing l E r = Node kMin vMin l cMin (delMin r)
                                              borrar clave mVal l c r = Node clave mVal l c r
                                              (kMin, vMin, cMin) = minTTree r


minTTree :: TTree k v -> (k, Maybe v, TTree k v)
minTTree (Leaf k v) = (k, Just v, E)
minTTree (Node k v E c _) = (k, v, c)
minTTree (Node _ _ l _ _) = minTTree l

delMin :: TTree k v -> TTree k v
delMin (Node _ _ E _ r) = r
delMin (Node k v l c r) = Node k v (delMin l) c r
delMin _ = E

{- arboles de prueba -}
t = Node 'r' Nothing E (Node 'e' (Just 16) (Node 'a' Nothing E (Leaf 's' 1) E)
                                          (Node 'o' (Just 2) (Leaf 'd' 9)
                                                              E
                                                              (Leaf 's' 4))
                                          E)
                      (Node 's' Nothing E (Node 'i' (Just 4) (Leaf 'e' 8)
                                                              (Leaf 'n' 7)
                                                              E)
                                          E)


j = Node 'h' (Just 3) E (Leaf 'o' 4) E

-- e)
class Dic k v d | d -> k v where
  vacio :: d
  insertar :: Ord k => k -> v -> d -> d
  buscar :: Ord k => k -> d -> Maybe v
  eliminar :: Ord k => k -> d -> d
  claves :: Ord k => d -> [k]

instance Ord k => Dic [k] v (TTree k v) where
  vacio = E
  insertar = insert
  buscar = search
  eliminar = delete
  claves = keys


{- Funciones interesantes: -}

-- fenixAux :: Ord k => [[k]] -> [v] -> TTree k v -> TTree k v
-- fenixAux [] [] tree         = tree
-- fenixAux (x:xs) (y:ys) tree = fenixAux xs ys (insert x y tree)
--
-- devuelve un TTree sin casos del tipo (Node clave Nothing l E r)
-- fenix :: Ord k => TTree k v -> TTree k v 
-- fenix tree = fenixAux claves valores tree
--             where
--              auxClaves = keys tree
--              (left, right) = splitAt ((length auxClaves + 1) `div` 2) auxClaves
--              merge [] ys = ys
--              merge (x:xs) ys = x:merge ys xs
--              claves = merge (reverse left) right
--              unJust [] = []
--              unJust ((Just x):xs) = x : unJust xs
--              valores = unJust (map (`search` tree) claves)

-- Una forma interesante, pero ineficiente, de eliminar un elemento del arbol
-- betterDelete arbol borrar = foldr (s insert (unJust . g search arbol)) E [x | x <- keys arbol, x /= borrar]
--                     where
--                       s f g x = f x (g x)
--                       unJust (Just x) = x
--                       g f x y = f y x
