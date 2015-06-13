module App.Util where

import           App.Types

import           System.Process
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.Char
import           GHC.IO.Handle
import           GHC.IO.Handle.FD
import           Text.Read
import           Text.RegexPR

hoistEitherIO :: IO (Either e a) -> EitherT e IO a
hoistEitherIO = hoistEither <=< liftIO

liftMaybe :: (MonadError e m) => e -> Maybe a -> m a
liftMaybe ex = maybe (throwError ex) return

orThrow :: (MonadError e m) => Maybe a -> e -> m a
orThrow = flip liftMaybe

orThrowM :: (MonadError e m) => m (Maybe a) -> e -> m a
orThrowM m e = m >>= liftMaybe e

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

openInBrowser :: String -> AppM ()
openInBrowser url = getConfig >>= liftIO . openInBrowser' url

openInBrowser' :: String -> Config -> IO ()
openInBrowser' url config =
  let command = view configBrowserCommand config
  in  void . createProcess . shell $ command ++ " '" ++ url ++ "'"


ask :: String -> IO String
ask question = putStrLn question >> putStr' "> " >> getLine'

runUserChoice :: String -> [(String, IO a)] -> IO a
runUserChoice question answers = do
  let question' = unlines $ question : renderAnswers
  answer <- readMaybe <$> ask question'
  case answer of
    Nothing -> tryAgain
    Just i  -> if i > 0 && i <= length answers
               then snd (answers !! (i - 1))
               else tryAgain
  where
    tryAgain = putStrLn "Invalid answer." >> runUserChoice question answers
    renderAnswers = zipWith renderAnswer [1..] $ map fst answers
    renderAnswer i answer = "[" ++ show i ++ "] " ++ answer

putStr' :: String -> IO ()
putStr' s = putStr s >> hFlush stdout

getLine' :: IO String
getLine' = trim <$> getLine

-- Match regex with string and return the first group match
(=~~) :: String -> String -> Maybe String
s =~~ regex = matchRegexPR regex s & view (_Just._2.to (lookup 1))
