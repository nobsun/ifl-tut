{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Stack
    where

import Data.Bool
import Data.List.Extra

data Stack a
    = Stack
    { maxDepth :: Int
    , curDepth :: Int
    , stkItems :: [a]
    } deriving Show

emptyStack :: Stack a
emptyStack = Stack 0 0 []

singletonStack :: a -> Stack a
singletonStack x = push x emptyStack

isEmptyStack :: Stack a -> Bool
isEmptyStack stk = null stk.stkItems

isSingletonStack :: Stack a -> Bool
isSingletonStack stk = not (isEmptyStack stk) 
                    && isEmptyStack (snd (pop stk))

push :: a -> Stack a -> Stack a
push x stk = stk
    { maxDepth = stk.maxDepth `max` succ stk.curDepth
    , curDepth = succ stk.curDepth
    , stkItems = x : stk.stkItems
    }

pop :: Stack a -> (a, Stack a)
pop stk = bool (list undefined phi stk.stkItems)
               (error "pop: empty stack")
               (isEmptyStack stk)
    where
        phi x xs = (x, stk { curDepth = pred stk.curDepth, stkItems = xs })

npop :: Int -> Stack a -> ([a], Stack a)
npop n stack = case n of
    0 -> ([], stack)
    _ -> case pop stack of
        (a, stack') -> case npop (pred n) stack' of
            (as, stack'') -> (a:as, stack'')

discard :: Int -> Stack a -> Stack a
discard 0 stk = stk
discard n stk = stk { curDepth = subtract n stk.curDepth `max` 0
                    , stkItems = drop n stk.stkItems
                    }

empty :: Stack a -> Stack a
empty stk = stk
    { curDepth = 0
    , stkItems = []
    }

saveStack :: (Stack a, Stack (Stack a)) -> (Stack a, Stack (Stack a))
saveStack (stk, dump) = case push stk dump of
    dump' -> case empty stk of
        stk' -> (stk', dump')

restoreStack :: (Stack a, Stack (Stack a)) -> (Stack a, Stack (Stack a))
restoreStack (stk, dump) = case pop dump of
    (stk', dump') -> (stk'', dump')
        where
            stk'' = stk' { maxDepth = stk.maxDepth `max` stk'.maxDepth }
