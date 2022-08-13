module Utils
    ( space, layn, rjustify
    , Assoc, aLookup, aDomain, aRange, aEmpty 
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

aDomain :: Assoc a b -> [a]
aDomain as = [ k | (k, _) <- as ]

aRange :: Assoc a b -> [b]
aRange as = [ v | (_, v) <- as ]

aEmpty :: Assoc a b
aEmpty = []
