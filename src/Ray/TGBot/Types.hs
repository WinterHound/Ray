{-# LANGUAGE GeneralisedNewtypeDeriving #-}


module Ray.TGBot.Types where


import           Control.Monad.Reader
import           Control.Monad.State


newtype TGBot reading state a = TGBot {
  runBot :: ReaderT reading (StateT state IO) a
  } deriving (Functor, Applicative, Monad,
                MonadIO,
                MonadFail,
                MonadReader reading,
                MonadState state)


runTGBot :: TGBot reading state a -> reading -> state -> IO a
runTGBot app read_ = evalStateT (runReaderT (runBot app) read_)
