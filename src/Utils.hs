module Utils
    ( space, layn, rjustify
    , Assoc, aLookup, aDomain, aRange, aEmpty, aInsert
    , merge
    )
    where

space :: Int -> String
space = (`replicate` ' ')

layn :: [String] -> String
layn = unlines . zipWith phi [1 :: Int ..]
    where
        phi i line = rjustify 4 (show i) ++ ')' : (space 3 ++ line)

rjustify :: Int -> String -> String        
rjustify w s = reverse $ take w $ reverse s ++ repeat ' '

{- | 連想リスト
-}
type Assoc a b = [(a, b)]

aLookup :: Eq a => Assoc a b -> a -> b -> b
aLookup []             _  def = def
aLookup ((k, v) : kvs) k' def
    | k == k'   = v
    | otherwise = aLookup kvs k' def

aInsert :: Eq a => Assoc a b -> a -> b -> Assoc a b
aInsert tab k v = (k, v) : tab

aDomain :: Assoc a b -> [a]
aDomain as = [ k | (k, _) <- as ]

aRange :: Assoc a b -> [b]
aRange as = [ v | (_, v) <- as ]

aEmpty :: Assoc a b
aEmpty = []

aAssocs :: Assoc a b -> [(a, b)]
aAssocs = id

{- | マージ
-}
merge :: Ord a => [a] -> [a] -> [a]
merge xxs yys = case xxs of
    [] -> yys
    x:xs -> case yys of
        [] -> xxs
        y:ys -> case compare x y of
            LT -> x : merge xs yys
            EQ -> x : merge xs ys
            _  -> y : merge xxs ys

