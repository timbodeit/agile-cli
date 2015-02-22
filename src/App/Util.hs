module App.Util where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.Char

hoistEitherIO :: IO (Either e a) -> EitherT e IO a
hoistEitherIO = hoistEither <=< liftIO

liftMaybe :: (MonadError e m) => e -> Maybe a -> m a
liftMaybe ex = maybe (throwError ex) return

liftEither :: (MonadError e m) => Either e a -> m a
liftEither = either throwError return

liftEitherIO :: (MonadError e m, MonadIO m) => IO (Either e a) -> m a
liftEitherIO ioe = liftIO ioe >>= either throwError return

liftEitherM :: (MonadError e m) => m (Either e a) -> m a
liftEitherM = (>>= either throwError return)

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

(>$<) :: Functor f => f a -> (a -> b) -> f b
(>$<) = flip fmap

(>$$<) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(>$$<) = flip (fmap . fmap)

(<$<) :: Functor f => (a -> b) -> (c -> f a) -> c -> f b
(<$<) f g = (f <$>) . g

trim :: String -> String
trim = go . go
  where go = reverse . dropWhile isSpace

toMaybe :: Either e a -> Maybe a
toMaybe = either (const Nothing) Just
