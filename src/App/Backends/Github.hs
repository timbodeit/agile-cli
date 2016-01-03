{-# LANGUAGE TypeFamilies #-}

module App.Backends.Github where

import           App.Backends.Types
import           App.CLI.Options
import           App.Git                   hiding (GitException)
import           App.Types
import           App.Util

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Data.Either.Combinators
import           Data.List
import           Data.Maybe
import qualified Github.Auth               as GH
import qualified Github.Issues             as GH
import qualified Github.Repos              as GH
import qualified Github.Search             as GH
import           Text.Read
import           Text.RegexPR

data GithubRepoRef = GithubRepoRef String String deriving (Eq, Show)

newtype GithubIssueId = GithubIssueId { unGithubIssueId :: Int } deriving Eq
newtype GithubIssueType = GithubIssueType { unGithubIssueType :: String } deriving Eq

instance Show GithubIssueId where
  show = show . unGithubIssueId

instance Show GithubIssueType where
  show = show . unGithubIssueType

instance IsIssueId GithubIssueId where

instance IsIssueType GithubIssueType where
  issueTypeName = unGithubIssueType
  issueTypeDescription _ = ""

instance IsIssue GH.Issue where
  type IssueId GH.Issue = GithubIssueId
  type IssueType GH.Issue = GithubIssueType
  type IssueTypeIdentifier GH.Issue = String

  issueId = GithubIssueId . GH.issueNumber
  issueStatus = maybe Open (const Closed) . GH.issueClosedAt
  issueType = GithubIssueType . GH.issueState

  summarize issue = unlines'
    [ "Issue Number: " ++ show (GH.issueNumber issue)
    , "ID: " ++ show (GH.issueId issue)
    , "Title: " ++ GH.issueTitle issue
    , "State: " ++ GH.issueState issue
    , "Labels: " ++ renderLabels (GH.issueLabels issue)
    , "Assignee: " ++ renderAssignee (GH.issueAssignee issue)
    , "Body:\n" ++ fromMaybe "" (GH.issueBody issue)
    ]
    where
      renderLabels   = intercalate ", " . map GH.labelName
      renderAssignee = maybe "-" GH.githubOwnerLogin

  summarizeOneLine issue = show (GH.issueNumber issue) ++ ": " ++ GH.issueTitle issue
  suggestedBranchName = GH.issueTitle

instance IssueBackend GithubConfig where
  type Issue GithubConfig = GH.Issue
  type IssueCreationData GithubConfig = GH.NewIssue

  createIssue creationData ghConfig = do
    GithubRepoRef owner repo <- issueRepositoryRef
    let auth = githubAuth ghConfig
    issue <- liftGithubIO $ GH.createIssue auth owner repo creationData
    return $ issueId issue

  getIssueById issueId _ = do
    repoRef <- issueRepositoryRef
    fetchIssue repoRef issueId

  makeIssueTransition (GithubIssueId issueNumber) transition ghConfig = do
    GithubRepoRef owner repo <- issueRepositoryRef
    let auth = githubAuth ghConfig
    let edit = GH.EditIssue Nothing Nothing Nothing (Just transition) Nothing Nothing
    void . liftGithubIO $
      GH.editIssue auth owner repo issueNumber edit

  close = flip makeIssueTransition "closed"

  reopen = flip makeIssueTransition "open"

  issueUrl issueId ghConfig = do
    issue <- getIssueById issueId ghConfig
    case GH.issueHtmlUrl issue of
      Nothing  -> throwError . IOException $ "Could not get issue URL for issue " ++ show issueId
      Just url -> return url

  parseIssueId s _ = liftMaybe ex $ GithubIssueId <$> readMaybe s
    where ex = IOException $ "Unable to parse issue ID: " ++ s

  extractIssueId branch _ = liftMaybe ex $ do
    s <- show branch =~~ "(\\d+)"
    GithubIssueId <$> readMaybe s
    where ex = IOException $ "Unable to extract issue ID from branch: " ++ show branch

  toIssueTypeIdentifier s _ = s

  getAvailableIssueTypes _ = return [GithubIssueType "(Any string)"]

  makeIssueCreationData issueType summary _ =
    GH.NewIssue summary Nothing Nothing Nothing (Just [issueType])

  searchIssues options s _ = do
    when optionsSelected $
      liftIO . putStrLn $ "Note: search options are currently ignored when using the Github backend"

    searchString <- issueSearchString s <$> issueRepositoryRef
    searchResult <- liftGithubIO $ GH.searchIssues searchString
    return $ GH.searchIssuesIssues searchResult
    where
      optionsSelected = searchOverAllProjects options || searchOnlyUserIssues options

  searchUrl options s _ = do
    when optionsSelected $
      liftIO . putStrLn $ "Note: search options are currently ignored when using the Github backend"

    repoRef@(GithubRepoRef owner repo) <- issueRepositoryRef
    let searchString = issueSearchString s repoRef
    return $ "https://github.com/" ++ owner ++ "/" ++ repo ++ "/issues?" ++ searchString
    where
      optionsSelected = searchOverAllProjects options || searchOnlyUserIssues options

  testBackend _ = do
    GithubRepoRef owner repo <- currentRepositoryRef
    liftIO . putStrLn $ "Using repository: " ++ owner ++ "/" ++ repo
    void . liftGithubIO $ GH.userRepo owner repo

issueSearchString :: String -> GithubRepoRef -> String
issueSearchString s (GithubRepoRef owner repo) = "q=" ++ (urlEncode $ s ++ " repo:" ++ owner ++ "/" ++ repo)

issueRepositoryRef :: AppM GithubRepoRef
issueRepositoryRef = do
  repoRef <- currentRepositoryRef
  repo <- fetchRepo repoRef
  let hasIssues = fromMaybe False $ GH.repoHasIssues repo
  let mParentRepo = forkedFromRepoRef repo

  return $
    case (hasIssues, mParentRepo) of
      (False, Just parentRepo) -> parentRepo
      _                        -> repoRef

currentRepositoryRef :: AppM GithubRepoRef
currentRepositoryRef = do
  config <- getConfig
  let ghConfig = config^.configGithubConfig
  let remote   = config^.configRemoteName

  refFromGithubConfig ghConfig <|||> refFromRemote remote
  where
    refFromGithubConfig :: GithubConfig -> AppM GithubRepoRef
    refFromGithubConfig ghConfig =
      let ref = GithubRepoRef <$> ghConfig^.githubUsername <*> ghConfig^.githubRepo
          ex  = IOException "Could not read github repo from config"
      in  liftMaybe ex ref

    refFromRemote :: String -> AppM GithubRepoRef
    refFromRemote remote = do
      url <- liftGit $ remoteUrl remote
      liftMaybe (extractException url) $ extractRepository url
      where
        extractException url =
          GitException $ "Unable to parse github repository from URL '" ++ url ++ "'"

extractRepository :: String -> Maybe GithubRepoRef
extractRepository url = do
  guard $ "github.com" `isInfixOf` url

  let match  = matchRegexPR "([^/:]+)/([^/]+?)(\\.git)?$" url
      groups = match^._Just._2
      user   = lookup 1 groups
      repo   = lookup 2 groups

  GithubRepoRef <$> user <*> repo

forkedFromRepoRef :: GH.Repo -> Maybe GithubRepoRef
forkedFromRepoRef repo = do
  guard =<< GH.repoFork repo
  parent <- GH.repoParent repo
  return $ fromRepoRef parent

githubAuth :: GithubConfig -> GH.GithubAuth
githubAuth = GH.GithubOAuth . view githubOAuthToken

fetchRepo :: GithubRepoRef -> AppM GH.Repo
fetchRepo (GithubRepoRef user repo) = liftGithubIO $ GH.userRepo user repo

fetchIssue :: GithubRepoRef -> GithubIssueId -> AppM GH.Issue
fetchIssue (GithubRepoRef owner repo) (GithubIssueId issueId) =
  liftGithubIO $ GH.issue owner repo issueId

liftGithub :: Either GH.Error a -> Either AppException a
liftGithub = mapLeft (IOException . show)

liftGithubIO :: IO (Either GH.Error a) -> AppM a
liftGithubIO = liftEitherIO . fmap liftGithub

fromRepoRef :: GH.RepoRef -> GithubRepoRef
fromRepoRef (GH.RepoRef owner repo) = GithubRepoRef (GH.githubOwnerLogin owner) repo
