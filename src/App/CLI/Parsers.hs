module App.CLI.Parsers where

import           App.Types
import           App.Util

import           Control.Applicative     ((<$>))
import           Control.Lens
import           Data.Either.Combinators
import           Jira.API                hiding (getConfig)
import           Text.Parsec

type StringParser a = Parsec String () a

issueNumberParser :: StringParser IssueNumber
issueNumberParser = IssueNumber . read <$> many1 digit

issueKeyParser :: StringParser IssueKey
issueKeyParser = do
  project <- manyTill letter (char '-')
  number  <- issueNumberParser
  return $ IssueKey project number

issueKeyParserWithDefaultProject :: String -> StringParser IssueKey
issueKeyParserWithDefaultProject project =
  issueKeyParser <|> IssueKey project <$> issueNumberParser

--

parseIssueKey :: String -> AppM IssueKey
parseIssueKey s = do
  project <- view (configJiraConfig.jiraProject) <$> getConfig
  liftEither $ mapLeft (const parseException) $
               parse (issueKeyParserWithDefaultProject project) "" s
  where
    parseException = UserInputException $
      "Not a valid issue identifier: " ++ s
