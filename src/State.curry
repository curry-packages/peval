--------------------------------------------------------------------------------
--- This module provides an implementation of the state monad.
---
--- @author Björn Peemöller
--- @version September 2015
--------------------------------------------------------------------------------
module State where

infixl 1 >+, >+=
infixl 4 <$>, <*>

type State s a = s -> (a, s)

--- Run a state monad action, yielding the result and the final state.
runState :: State s a -> s -> (a, s)
runState state s = state s

--- Evaluate a state monad action, yielding only the result.
evalState :: State s a -> s -> a
evalState state s = fst (runState state s)

--- Execute a state monad action, yielding only the result.
execState :: State s a -> s -> s
execState state s = snd (runState state s)

--- Lifting operation.
returnS :: a -> State s a
returnS x s = (x, s)

--- Bind operation.
(>+=) :: State s a -> (a -> State s b) -> State s b
(m >+= f) s = case m s of (x, s') -> f x s'

--- Sequence operation.
(>+) :: State s a -> State s b -> State s b
m >+ n = m >+= \_ -> n

--- Strict sequence operation, useful for debugging.
(>!) :: State s () -> State s b -> State s b
m >! n = m >+= \() -> n

--- Apply a pure function to the result of a monadic action.
(<$>) :: (a -> b) -> State s a -> State s b
f <$> act = act >+= returnS . f

--- Apply a function from a monad to the result of another monadic action.
(<*>) :: State s (a -> b) -> State s a -> State s b
a <*> b = a >+= \f -> b >+= \x -> returnS (f x)

--- Retrieve the internal state.
getS :: State s s
getS = getsS id

--- Retrieve a part of the internal state by an accessor function.
getsS :: (s -> t) -> State s t
getsS f s = (f s, s)

--- Set the internal state.
putS :: s -> State s ()
putS s _ = ((), s)

--- Modify the internal state using the given function.
modifyS :: (s -> s) -> State s ()
modifyS f s = ((), f s)

--- Sequence a list of actions and return their results.
sequenceS :: [State s a] -> State s [a]
sequenceS =
 foldr (\s newS -> s    >+= \a  ->
                   newS >+= \as ->
                   returnS (a:as))
       (returnS [])

--- Sequence a list of actions but ignore their results.
sequenceS_ :: [State s a] -> State s ()
sequenceS_ = foldr (>+) (returnS ())

--- Apply a monadic action to each element of the list and collect the results.
mapS :: (a -> State s b) -> [a] -> State s [b]
mapS f = sequenceS . map f

--- Apply a monadic action to each element of the list but ignore the results.
mapS_ :: (a -> State s b) -> [a] -> State s ()
mapS_ f = sequenceS_ . map f

--- Monadic version of `concatMap`.
concatMapS :: (a -> State s [b]) -> [a] -> State s [b]
concatMapS f xs = concat <$> mapS f xs
