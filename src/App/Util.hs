{-# LANGUAGE LambdaCase #-}

module App.Util where

import           App.Types

import           System.Process
import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Either
import           Data.Char
import           GHC.IO.Handle
import           GHC.IO.Handle.FD
import           Text.Read
import           Text.RegexPR

-- Error handling

toMaybe :: Either e a -> Maybe a
toMaybe = either (const Nothing) Just

toEither :: b -> Maybe a -> Either b a
toEither b = maybe (Left b) Right

hoistEitherIO :: IO (Either e a) -> EitherT e IO a
hoistEitherIO = hoistEither <=< liftIO

liftMaybe :: (MonadError e m) => e -> Maybe a -> m a
liftMaybe ex = maybe (throwError ex) return

tryMaybe :: (MonadError e m) => m a -> m (Maybe a)
tryMaybe m = liftM Just m `orElse` return Nothing

onError :: (MonadError e m) => m a -> m a -> m a
onError = flip catchError . const

orThrow :: (MonadError e m) => Maybe a -> e -> m a
orThrow = flip liftMaybe

orThrowM :: (MonadError e m) => m (Maybe a) -> e -> m a
orThrowM m e = m >>= liftMaybe e

orElse :: (MonadError e m) => m a -> m a -> m a
orElse m d = m `catchError` const d

liftEither :: (MonadError e m) => Either e a -> m a
liftEither = either throwError return

liftEitherIO :: (MonadError e m, MonadIO m) => IO (Either e a) -> m a
liftEitherIO ioe = liftIO ioe >>= either throwError return

liftEitherM :: (MonadError e m) => m (Either e a) -> m a
liftEitherM = (>>= either throwError return)

-- Custom operators

(>>>) :: a -> (a -> b) -> b
(>>>) = flip ($)

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

(>$<) :: Functor f => f a -> (a -> b) -> f b
(>$<) = flip fmap

(>$$<) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(>$$<) = flip (fmap . fmap)

(<$<) :: Functor f => (a -> b) -> (c -> f a) -> c -> f b
(<$<) f g = (f <$>) . g

-- String handling

trim :: String -> String
trim = go . go
  where go = reverse . dropWhile isSpace

-- User interaction

openInBrowser :: String -> AppM ()
openInBrowser url = getConfig >>= liftIO . openInBrowser' url

openInBrowser' :: String -> Config -> IO ()
openInBrowser' url config =
  let command = view configBrowserCommand config
  in  void . createProcess . shell $ command ++ " '" ++ url ++ "'"

ask :: String -> IO String
ask question = putStrLn question >> putStr' "> " >> getLine'

askWithDefault :: String -> String -> IO String
askWithDefault defaultAnswer question =
  ask (question ++ " [" ++ defaultAnswer ++ "]") >$< \case
    "" -> defaultAnswer
    s  -> s

askYesNoWithDefault :: Bool -> String -> IO Bool
askYesNoWithDefault defaultAnswer question = do
  let question' = question ++ " [" ++ showChoices ++ "]"
  putStrLn question'
  putStr' "> "
  getChar' >>= \case
    c | c `elem` "yY"   -> return True
      | c `elem` "nN"   -> return False
      | c `elem` "\n\r" -> return defaultAnswer
      | otherwise -> putStrLn
         ("Please answer with y or n or press enter for the default (" ++ showDefault ++ ")")
         >> tryAgain
  where
    showChoices = if defaultAnswer then "Y/n" else "y/N"
    showDefault = if defaultAnswer then "y" else "n"
    tryAgain    = askYesNoWithDefault defaultAnswer question

runUserChoice :: String -> [(String, IO a)] -> IO a
runUserChoice question answers = do
  let question' = unlines' $ question : renderAnswers
  readMaybe <$> ask question' >>= \case
    Nothing -> tryAgain
    Just i | i > 0 && i <= length answers -> snd (answers !! (i - 1))
           | otherwise -> tryAgain
  where
    tryAgain = putStrLn "Invalid answer." >> runUserChoice question answers
    renderAnswers = zipWith renderAnswer [1..] $ map fst answers

    renderAnswer :: Int -> String -> String
    renderAnswer i answer = "[" ++ show i ++ "] " ++ answer

putStr' :: String -> IO ()
putStr' s = putStr s >> hFlush stdout

getLine' :: IO String
getLine' = trim <$> getLine

getChar' :: IO Char
getChar' = do
  savedBuffering <- hGetBuffering stdin
  bracket_ (hSetBuffering stdin NoBuffering) (hSetBuffering stdin savedBuffering) $ do
    c <- getChar
    -- Continue writing to terminal on new line
    putStrLn ""
    return c

-- Like unlines but without the final newline
unlines' :: [String] -> String
unlines' [] = ""
unlines' l  = init (unlines l)

-- Regular Expression

-- Match regex with string and return the first group match
(=~~) :: String -> String -> Maybe String
s =~~ regex = matchRegexPR regex s & view (_Just._2.to (lookup 1))
