{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module App.Util where

import           App.Types

import           qualified Data.ByteString as BS
import           Network.HTTP.Types.URI    (urlEncode)
import           Data.String.Conversions
import           System.Process
import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Maybe
import           Data.Char
import           Data.List
import           Data.List.Split
import           GHC.IO.Handle
import           GHC.IO.Handle.FD
import           Text.Read
import           Text.RegexPR

-- Error handling

toMaybe :: Either e a -> Maybe a
toMaybe = either (const Nothing) Just

toEither :: b -> Maybe a -> Either b a
toEither b = maybe (Left b) Right

liftMaybe :: (MonadError e m) => e -> Maybe a -> m a
liftMaybe ex = maybe (throwError ex) return

tryMaybe :: (MonadError e m) => m a -> m (Maybe a)
tryMaybe m = fmap Just m `orElse` return Nothing

hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return

onError :: (MonadError e m) => m a -> m a -> m a
onError = flip catchError . const

orElse :: (MonadError e m) => m a -> m a -> m a
orElse = flip onError

orThrow :: (MonadError e m) => Maybe a -> e -> m a
orThrow = flip liftMaybe

orThrowM :: (MonadError e m) => m (Maybe a) -> e -> m a
orThrowM m e = m >>= liftMaybe e

attempt :: (Functor m, MonadError e m) => m a -> m ()
attempt m = void m `orElse` return ()

liftEither :: (MonadError e m) => Either e a -> m a
liftEither = either throwError return

liftEitherIO :: (MonadError e m, MonadIO m) => IO (Either e a) -> m a
liftEitherIO ioe = liftIO ioe >>= either throwError return

liftEitherM :: (MonadError e m) => m (Either e a) -> m a
liftEitherM = (>>= either throwError return)

mapError :: (Monad m, MonadError e m) => (e -> e) -> m a -> m a
mapError f = (`catchError` throwError . f)

orMap :: (Monad m, MonadError e m) => m a -> (e -> e) -> m a
orMap = flip mapError

-- Custom operators

(>>>) :: a -> (a -> b) -> b
(>>>) = flip ($)

infixl 4 <$$>
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

infixr 4 >$<
(>$<) :: Functor f => f a -> (a -> b) -> f b
(>$<) = flip fmap

infixr 4 >$$<
(>$$<) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(>$$<) = flip (fmap . fmap)

infixr 4 <$<
(<$<) :: Functor f => (a -> b) -> (c -> f a) -> c -> f b
(<$<) f g = (f <$>) . g

infixl 3 <||>
(<||>) :: (Alternative a, Monad m, Eq (a b)) => m (a b) -> m (a b) -> m (a b)
ma <||> mb = do
  a <- ma
  if a == empty
  then mb
  else ma

infixl 3 <|||>
(<|||>) :: (MonadError e m) => m a -> m a -> m a
ma <|||> mb = ma `catchError` const mb

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
  in  void . createProcess . shell $ command ++ " '" ++ escape url ++ "'"
  where
    escape = replace "'" "'\"'\"'"
    replace old new = intercalate new . splitOn old

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

userChoice :: String -> [(String, a)] -> IO a
userChoice question answers = do
  let question' = unlines' $ question : renderAnswers
  readMaybe <$> ask question' >>= \case
    Just i | i > 0 && i <= length answers -> return . snd $ answers !! (i - 1)
    _ -> tryAgain
  where
    tryAgain = putStrLn "Invalid answer." >> userChoice question answers
    renderAnswers = zipWith renderAnswer [1..] $ map fst answers
    renderAnswer i answer = "[" ++ show i ++ "] " ++ answer

runUserChoice :: String -> [(String, IO a)] -> IO a
runUserChoice question = join . userChoice question

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

-- URL-encode a given string
urlEncode :: (ConvertibleStrings a BS.ByteString, ConvertibleStrings BS.ByteString a) => a -> a
urlEncode = cs . Network.HTTP.Types.URI.urlEncode True . cs
