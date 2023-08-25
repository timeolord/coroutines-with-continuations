{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE InstanceSigs #-}
module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Cont
import           Control.Monad.Identity
import           Control.Monad.State
import           Data.Map.Strict
import           Data.Maybe
import           Control.Concurrent

newtype Coroutine r m a = Coroutine { runCoroutine' :: ContT r (StateT [Coroutine r m ()] m) a }

instance Functor m => Functor (Coroutine r m) where
    fmap f (Coroutine c) = Coroutine $ fmap f c

instance Applicative m => Applicative (Coroutine r m) where
    pure = Coroutine . pure
    (Coroutine f) <*> (Coroutine a) = Coroutine $ f <*> a

instance Monad m => Monad (Coroutine r m) where
    (Coroutine a) >>= f = Coroutine $ a >>= runCoroutine' . f

instance Monad m => MonadCont (Coroutine r m) where
    callCC :: Monad m => ((a -> Coroutine r m b) -> Coroutine r m a) -> Coroutine r m a
    callCC f = Coroutine $ callCC $ \k -> runCoroutine' (f (Coroutine . k))

instance MonadIO m => MonadIO (Coroutine r m) where
    liftIO :: MonadIO m => IO a -> Coroutine r m a
    liftIO = Coroutine . liftIO

instance MonadTrans (Coroutine r) where
    lift :: Monad m => m a -> Coroutine r m a
    lift = Coroutine . lift . lift

instance MonadState s m => MonadState s (Coroutine r m) where
    get :: MonadState s m => Coroutine r m s
    get = lift get
    put :: MonadState s m => s -> Coroutine r m ()
    put = lift . put

getContinuations :: Monad m => Coroutine r m [Coroutine r m ()]
getContinuations = Coroutine get

putContinuations :: Monad m => [Coroutine r m ()] -> Coroutine r m ()
putContinuations = Coroutine . put

queue :: Monad m => Coroutine r m () -> Coroutine r m ()
queue p = do
    ccs <- getContinuations
    putContinuations $ ccs ++ [p]

schedule :: Monad m => Coroutine r m ()
schedule = do
    cont <- getContinuations
    case cont of
        []     -> return ()
        (p:ps) -> do 
            putContinuations ps
            p

yield :: Monad m => Coroutine r m ()
yield = callCC $ \k -> do 
    queue (k ())
    schedule

fork :: Monad m => Coroutine r m () -> Coroutine r m ()
fork p = callCC $ \k -> do
     queue (k ())
     p
     schedule

finished :: Monad m => Coroutine r m ()
finished = do
  remaining <- Prelude.null <$> getContinuations
  do
    unless remaining Main.yield
    finished

runCoroutine :: Monad m => Coroutine r m r -> m r
runCoroutine coroutine = evalStateT (runContT (runCoroutine' $ coroutine <* finished) return) []

type GameStateT m = StateT GameState m
type Length = Int
type Name = String
type GameState = Map Name Length

wormState :: (MonadIO m, Monad m) => Name -> Coroutine () (GameStateT m) ()
wormState name = do
    modify (insert name 4)
    replicateM_ 12 $ do
        Main.yield
        modify (adjustWithKey (const succ) name)
    replicateM_ 12 $ do
        Main.yield
        modify (adjustWithKey (const pred) name)

render :: (MonadIO m, Monad m) => Name -> Coroutine () (GameStateT m) ()
render name = do
    l <- get
    let string = concat $ maybe [] (\x -> replicate (x + 1) "#") (Data.Map.Strict.lookup name l)
    liftIO $ putStr "\ESC[2J"
    liftIO $ putStrLn string
    liftIO $ threadDelay 50000
    Main.yield

gameCoroutine :: Coroutine () (GameStateT IO) ()
gameCoroutine = do
    let wormName = "Níðhöggr"
    fork . forever . wormState $ wormName
    fork . forever . render $ wormName
    
main = execStateT (runCoroutine gameCoroutine) Data.Map.Strict.empty