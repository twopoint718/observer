module Main where

import Control.Monad.State.Lazy (get, put, liftM, lift, modify, StateT(..))
import Control.Arrow (second)

newtype Observer a m = Observer { getO :: a -> SubjectT a m () }
type SubjectT a m b = StateT (a, [Observer a m]) m b

setSubject :: Monad m => a -> SubjectT a m ()
setSubject x = do
  (_, observers) <- get
  put (x, observers)

getSubject :: Monad m => SubjectT a m a
getSubject = liftM fst get

addObserver :: Monad m => Observer a m -> SubjectT a m ()
addObserver observer = modify $ second ((:) observer)

notify :: Monad m => SubjectT a m ()
notify = StateT $ \(a, os) ->
  let notifyAux []     = return ((), (a, os))
      notifyAux (o':os') = do
        (_, obs) <- notifyAux os'
        runStateT (getO o' a) obs
  in
      case os of
        [] -> return ((), (a, os))
        _  -> notifyAux os

observer1 :: Observer String IO
observer1 = Observer $ \x ->
  lift $ putStrLn x

observer2 :: Observer String IO
observer2 = Observer $ \x ->
  lift $ appendFile "./output.log" (x ++ "\n")

program :: SubjectT String IO ()
program = do
  addObserver observer1
  addObserver observer2
  setSubject "cats!"
  notify
  setSubject "dogs!"
  notify

main :: IO ()
main = do
  _ <- runStateT program ("", [])
  return ()
