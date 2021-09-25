module Stack where
import GHC.Show (Show)

data Stack a 
    = Stack
    { maxDepth_ :: Int
    , depth_    :: Int
    , stack_    :: [a]
    } deriving (Show)

emptyStack :: Stack a
emptyStack = Stack 0 0 []

isEmptyStack :: Stack a -> Bool
isEmptyStack stk = null (stack_ stk)

push :: a -> Stack a -> Stack a
push x stk = case stk of
    Stack maxDepth depth stack -> Stack (maxDepth `max` depth') depth' (x : stack)
        where
            depth' = succ depth

pop :: Stack a -> (a, Stack a)
pop stk = case stk of
    Stack maxDepth depth (x:stack) -> (x, Stack maxDepth (pred depth) stack)
    _                              -> error "Empty stack"

discard :: Int -> Stack a -> Stack a
discard 0 stk = stk
discard n stk = case stk of
    Stack maxDepth depth stack
        -> Stack maxDepth (subtract n depth `max` 0) (drop n stack)