{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- This is mostly for the TH staging restrictions
module Howl.Types where

import           Data.Aeson
import           Data.Text
import           Database.Persist.TH
import           GHC.Generics
import           Prelude
import           Network.HTTP.Conduit     (Manager)
import           Database.Persist.Sql (ConnectionPool)

import           Control.Monad.Base          (MonadBase)
import           Control.Monad.Except        (MonadError, catchError,
                                              throwError)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Logger        (LogLevel (..), MonadLogger (..),
                                              logDebug, logError, logInfo,
                                              logWarn, toLogStr)
import           Control.Monad.Reader        (MonadReader, ReaderT, ask,
                                              runReaderT)
import qualified Control.Monad.Reader        as Reader (asks)
import           Control.Monad.RWS           (RWST)
import           Control.Monad.State         (StateT)
import           Control.Monad.Trans.Class   (MonadTrans (..))
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..),
                                              defaultLiftBaseWith,
                                              defaultRestoreM)
import           Control.Monad.Trans.Either  (EitherT)
import           Control.Monad.Trans.Except  (ExceptT, runExceptT)
import Control.Monad.Writer (WriterT)

import           Servant.API
import qualified Howl.Facebook       as FB
import           Howl.Logger (LogFunction, withLogger)

-- FollowStatus
data FollowStatus = Pending | Accepted | Ignored | Blocked
  deriving (Show, Read, Eq, Generic)

instance ToJSON FollowStatus where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON FollowStatus
derivePersistField "FollowStatus"

type Token = Text

instance ToHttpApiData FB.Id where
  toUrlPiece = FB.idCode

instance FromHttpApiData FB.Id where
  parseUrlPiece = Right . FB.Id


data HandlerEnv = HandlerEnv
  { db :: ConnectionPool
  , manager :: Manager
  , creds :: FB.Credentials
  }

-- Diener
data LogEnv r = LogEnv
  { logFunction :: LogFunction
  , logEnv      :: r
  }

newtype DienerT e r m a
  = DienerT { unDienerT :: ExceptT e (ReaderT (LogEnv r) m) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadError e
             , MonadReader (LogEnv r)
             )

instance MonadIO m => MonadLogger (DienerT e r m) where
  monadLoggerLog loc src lvl msg = do
    f <- Reader.asks logFunction
    liftIO $ f loc src lvl $ toLogStr msg

deriving instance (MonadBase b m) => MonadBase b (DienerT e r m)

instance MonadTrans (DienerT e r) where
  lift = DienerT . lift . lift

instance MonadBaseControl b m => MonadBaseControl b (DienerT e r m) where
  type StM (DienerT e r m) a = ComposeSt (DienerT e r) m a
  liftBaseWith     = defaultLiftBaseWith
  restoreM         = defaultRestoreM

instance MonadTransControl (DienerT e r) where
  type StT (DienerT e r) a = StT (ExceptT e) (StT (ReaderT r) a)
  liftWith f = DienerT $ liftWith $ \run ->
                                    liftWith $ \run' ->
                                                f (run' . run . unDienerT)
  restoreT = DienerT . restoreT . restoreT

runDienerT :: LogEnv r
           -> DienerT e r m a
           -> m (Either e a)
runDienerT env (DienerT m)
  = runReaderT (runExceptT m) env

class (Monad m, Monad io) => MonadDiener e r m io where
  diener :: DienerT e r io a -> m a

instance Monad io => MonadDiener e r (DienerT e r io) io where
  diener = id

instance (MonadDiener e r m io) => MonadDiener e r (ReaderT r m) io where
  diener = lift . diener

instance (MonadDiener e r m io, Monoid w) => MonadDiener e r (WriterT w m) io where
  diener = lift . diener

instance (MonadDiener e r m io) => MonadDiener e r (StateT s m) io where
  diener = lift . diener

instance (MonadDiener e r m io) => MonadDiener e r (EitherT e' m) io where
  diener = lift . diener

instance (MonadDiener e r m io) => MonadDiener e r (ExceptT e' m) io where
  diener = lift . diener

instance (Monoid w, MonadDiener e r m io) => MonadDiener e r (RWST r' w s m) io where
  diener = lift . diener

asks :: Monad m => (r -> a) -> DienerT e r m a
asks f = f <$> Reader.asks logEnv
