--------------------------------------------------------------------------------
--- This module provides an implementation of non-deterministic state monad
--- where the non-determinism distributes over state.
---
--- @author  Björn Peemöller
--- @version September 2015
--------------------------------------------------------------------------------
module NDState where

import Prelude hiding ((<$>))

infixl 1 >+, >!, >+=
infixl 3 <|>
infixl 4 <$>, <*>

--- Non-Deterministic result
--- @cons Return - single result
--- @cons Choice - non-deterministic choice
data Result a
  = Return a
  | Choice (Result a) (Result a)

--- Monadic bind operation.
bindResult :: Result a -> (a -> Result b) -> Result b
bindResult (Return   x) f = f x
bindResult (Choice a b) f = Choice (bindResult a f) (bindResult b f)

--- Non-Deterministic state monad.
type State s a = s -> Result (a, s)

--- Run a monadic computation.
runState :: State s a -> s -> Result (a, s)
runState state s = state s

--- Monadic bind for state monad.
(>+=) :: State s a -> (a -> State s b) -> State s b
(m >+= f) s = bindResult (runState m s) (\(x, s') -> runState (f x) s')

--- Monadic sequence for state monad.
(>+) :: State s a -> State s b -> State s b
m >+ n = m >+= \_ -> n

--- Strict monadic sequence for state monad.
(>!) :: State s () -> State s b -> State s b
m >! n = m >+= \() -> n

--- Monadic return for state monad.
returnS :: a -> State s a
returnS x s = Return (x, s)

--- Non-Deterministic choice for state monad.
choiceS :: State s a -> State s a -> State s a
choiceS a b s = Choice (runState a s) (runState b s)

--- Retrieve the state from the state monad.
getS :: State s s
getS = getsS id

--- Retrieve the state from the state monad using an accessor function.
getsS :: (s -> t) -> State s t
getsS f s = Return (f s, s)

--- Set the state from the state monad.
putS :: s -> State s ()
putS s _ = Return ((), s)

--- Modify the state from the state monad using a function.
modifyS :: (s -> s) -> State s ()
modifyS f s = Return ((), f s)

--- Sequence a list of monadic computations and collect the results.
sequenceS :: [State s a] -> State s [a]
sequenceS =
 foldr (\s newS -> s    >+= \a  ->
                   newS >+= \as ->
                   returnS (a:as))
       (returnS [])

--- Sequence a list of monadic computations and discard the results.
sequenceS_ :: [State s a] -> State s ()
sequenceS_ = foldr (>+) (returnS ())

--- Perform a monadic computations for each element in a list
--- and collect the results.
mapS :: (a -> State s b) -> [a] -> State s [b]
mapS f = sequenceS . map f

--- Perform a monadic computations for each element in a list
--- and discard the results.
mapS_ :: (a -> State s b) -> [a] -> State s ()
mapS_ f = sequenceS_ . map f

--- Apply a pure function to the result of a monadic action.
(<$>) :: (a -> b) -> State s a -> State s b
(<$>) f act = act >+= \x -> returnS (f x)

--- Apply a function originating from the first monadic computation
--- to the result of the second monadic action.
(<*>) :: State s (a -> b) -> State s a -> State s b
a <*> b = a >+= \f -> b >+= \x -> returnS (f x)

--- Infix version of non-deterministic choice.
(<|>) :: State s a -> State s a -> State s a
(<|>) = choiceS

--- Monadic version of `any`.
anyS :: (a -> State s Bool) -> [a] -> State s Bool
anyS p xs = or <$> mapS p xs

--- Monadic version of `all`.
allS :: (a -> State s Bool) -> [a] -> State s Bool
allS p xs = and <$> mapS p xs
