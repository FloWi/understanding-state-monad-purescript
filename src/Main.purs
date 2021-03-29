module Main where

import Control.Monad.State (State, get, modify_, runState)
import Data.List (List(..), drop, head, (:))
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Prelude (class Applicative, class Apply, class Bind, class Functor, Unit, bind, discard, pure, unit, ($))
import Effect (Effect)
import Effect.Console (logShow)

type Stack
  = List Int

push :: Int -> Stack -> Stack
push x st = x : st

pop :: Stack -> Tuple (Maybe Int) Stack
pop xs = Tuple (head xs) (drop 1 xs)

newtype Stat3 s v
  = Stat3 (s -> Tuple v s)

runStat3 :: forall s v. Stat3 s v -> s -> Tuple v s
runStat3 (Stat3 g) s = g s

instance functorStat3 :: Functor (Stat3 s) where
  -- map :: forall a b. (a -> b) -> f a -> f b
  map g f = Stat3 (\s -> let Tuple v s' = runStat3 f s in Tuple (g v) s')

instance applyStat3 :: Functor (Stat3 s) => Apply (Stat3 s) where
  -- apply :: forall a b. f (a -> b) -> f a -> f b
  apply fg f =
    Stat3
      ( \s ->
          let
            Tuple g s' = runStat3 fg s

            Tuple v s'' = runStat3 f s'
          in
            Tuple (g v) s''
      )

instance applicativeStat3 :: Apply (Stat3 s) => Applicative (Stat3 s) where
  -- pure :: forall a. a -> f a
  pure v = Stat3 (\s -> Tuple v s)

instance bindStat3 :: Apply (Stat3 s) => Bind (Stat3 s) where
  -- bind :: forall a b. m a -> (a -> m b) -> m b
  bind m g = Stat3 (\s -> let Tuple v s' = runStat3 m s in runStat3 (g v) s')

pushSt4t3 :: Int -> Stat3 (List Int) Unit
pushSt4t3 x = Stat3 (\s -> Tuple unit (x : s))

popSt4t3 :: Stat3 (List Int) (Maybe Int)
popSt4t3 = Stat3 (\s -> Tuple (head s) (drop 1 s))

g3t :: forall s. Stat3 s s
g3t = Stat3 (\s -> Tuple s s)

m0dify_ :: forall s. (s -> s) -> Stat3 s Unit
m0dify_ g = do
  s <- g3t
  Stat3 (\s -> Tuple unit (g s))

popStat3 :: Stat3 (List Int) (Maybe Int)
popStat3 = do
  xs <- g3t
  m0dify_ $ drop 1
  pure $ head xs

pushStat3 :: Int -> Stat3 (List Int) Unit
pushStat3 x = m0dify_ (\s -> x : s)

m4nip :: Stat3 (List Int) Unit
m4nip = do
  pushStat3 4
  _ <- popStat3
  _ <- popStat3
  pure unit

pushState :: Int -> State (List Int) Unit
pushState x = modify_ (\s -> x : s)

popState :: State (List Int) (Maybe Int)
popState = do
  xs <- get
  modify_ $ drop 1
  pure $ head xs

manip :: State (List Int) Unit
manip = do
  pushState 4
  _ <- popState
  _ <- popState
  pure unit

main :: Effect Unit
main = do
  let
    stack = 3 : 2 : 1 : Nil

    stack4 = push 4 stack

    Tuple m4 stack3 = pop stack4

    Tuple m3 stack2 = pop stack3
  logShow stack
  -- (3 : 2 : 1 : Nil)
  logShow stack4
  -- (4 : 3 : 2 : 1 : Nil)
  logShow m4
  -- (Just 4)
  logShow stack3
  -- (3 : 2 : 1 : Nil)
  logShow m3
  -- (Just 3)
  logShow stack2
  -- (2 : 1 : Nil)
  logShow $ runStat3 m4nip (3 : 2 : 1 : Nil)
  -- (2 : 1 : Nil)
  logShow $ runState manip (3 : 2 : 1 : Nil)

-- (2 : 1 : Nil)
