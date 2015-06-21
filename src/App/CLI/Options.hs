{-# LANGUAGE TemplateHaskell #-}

module App.CLI.Options where

import           Control.Lens
import           Options.Applicative

data Verbosity = NonVerbose
               | Verbose
               deriving (Show, Eq, Ord, Enum)

data FinishType = FinishWithPullRequest
                | FinishWithMerge
                deriving (Show, Eq)

data SearchOptions = SearchOptions { searchOverAllProjects :: Bool
                                   , searchOnlyUserIssues  :: Bool
                                   , searchOnWebsite       :: Bool
                                   } deriving (Show, Eq)

data CLICommand = InitCommand
                | ConfigTestCommand
                | ShowIssueTypesCommand
                | CleanupBranchesCommand
                | ShowCommand (Maybe String)
                | OpenCommand (Maybe String)
                | SearchCommand SearchOptions String
                | NewCommand Bool String String
                | StartCommand (Maybe String)
                | StopCommand (Maybe String)
                | ResolveCommand (Maybe String)
                | CloseCommand (Maybe String)
                | ReopenCommand (Maybe String)
                | CheckoutCommand String
                | CreatePullRequestCommand (Maybe String)
                | FinishCommand FinishType (Maybe String)
                | CommitCommand [String]
                deriving (Show, Eq)

data CLIOptions = CLIOptions
  { _cliConfigFilePath :: Maybe FilePath
  , _cliVerbosity      :: Verbosity
  , _cliCommand        :: CLICommand
  } deriving (Show, Eq)

makeLenses ''CLIOptions

optionParser :: Parser CLIOptions
optionParser = CLIOptions
  <$> optional (
    strOption ( short 'c'
             <> long "config"
             <> metavar "CONFIG"
             <> help "Config file path"
              ))
  <*> flag NonVerbose Verbose
      (  short 'v'
      <> long "verbose"
      <> help "Whether to be verbose"
      )
  <*> subparser commands
  where
    commands =
        command "init" (toParserInfo "Initialize config file" initCommandParser)
     <> command "test" (toParserInfo "Test config" configTestCommandParser)
     <> command "issuetypes" (toParserInfo "Show available issue types"
        (pure ShowIssueTypesCommand))
     <> command "cleanup" (toParserInfo "Remove fully merged and closed local branches"
        (pure CleanupBranchesCommand))
     <> command "show" (toParserInfo "Print information about an issue to stdout" $
        ShowCommand <$> issueArgParser)
     <> command "open" (toParserInfo "Open link to an issue in browser" $
        OpenCommand <$> issueArgParser)
     <> command "start" (toParserInfo "Start work on an issue" $
        StartCommand <$> issueArgParser)
     <> command "stop" (toParserInfo "Stop work on an issue" $
        StopCommand <$> issueArgParser)
     <> command "resolve" (toParserInfo "Mark an issue as resolved" $
        ResolveCommand <$> issueArgParser)
     <> command "close" (toParserInfo "Close an issue" $
        CloseCommand <$> issueArgParser)
     <> command "reopen" (toParserInfo "Reopen an issue" $
        ReopenCommand <$> issueArgParser)
     <> command "pr" (toParserInfo "Create a pull request for an issue's branch" $
        CreatePullRequestCommand <$> issueArgParser)
     <> command "finish" (toParserInfo "Finish an issue (resolve and PR)"
        finishCommandParser)
     <> command "search" (toParserInfo "Search for issues" searchCommandParser)
     <> command "new" (toParserInfo "Create a new issue" newCommandParser)
     <> command "co" (toParserInfo "Checkout an issue's branch" checkoutCommandParser)
     <> command "commit" (toParserInfo "Create git commit" commitCommandParser)

initCommandParser :: Parser CLICommand
initCommandParser = pure InitCommand

configTestCommandParser :: Parser CLICommand
configTestCommandParser = pure ConfigTestCommand

searchCommandParser :: Parser CLICommand
searchCommandParser = SearchCommand
  <$> searchOptionsParser
  <*> (restArgs ( metavar "JQL"
               <> help "Search query in JQL"
                ) <|> pure ""
      )
  where
    searchOptionsParser :: Parser SearchOptions
    searchOptionsParser = SearchOptions
      <$> switch ( short 'a'
                <> long "all"
                <> help "Whether to search over all projects"
                 )
      <*> switch ( short 'm'
                <> long "my"
                <> help "Whether to only search issues assigned to you"
                 )
      <*> switch ( short 'b'
                <> long "browser"
                <> help "Whether show search results in the browser"
                 )

newCommandParser :: Parser CLICommand
newCommandParser = NewCommand
  <$> switch ( short 's'
            <> long "start"
            <> help "Whether to start the issue after creation"
             )
  <*> strArgument ( metavar "ISSUETYPE"
                 <> help "Issue type (alias) - use the issuetypes command for available options"
                  )
  <*> restArgs ( metavar "SUMMARY"
              <> help "Issue summary"
               )

checkoutCommandParser :: Parser CLICommand
checkoutCommandParser = CheckoutCommand <$> strArgument
  ( metavar "ISSUE"
 <> help "Issue key or number"
  )

finishCommandParser :: Parser CLICommand
finishCommandParser = FinishCommand
  <$> flag FinishWithPullRequest FinishWithMerge
      ( short 'm'
     <> long "merge"
     <> help "Whether to merge branch directly instead of creating a pull request"
      )
  <*> issueArgParser

commitCommandParser :: Parser CLICommand
commitCommandParser = CommitCommand
  <$> many (strArgument (help "Additional options to git commit"))

issueArgParser :: Parser (Maybe String)
issueArgParser = optional $ strArgument
  ( metavar "ISSUE"
 <> help "Issue key or number"
  )

toParserInfo :: String -> Parser a -> ParserInfo a
toParserInfo desc p = info (helper <*> p) (fullDesc <> progDesc desc)

restArgs :: Mod ArgumentFields String -> Parser String
restArgs opts = unwords <$> some (strArgument opts)
