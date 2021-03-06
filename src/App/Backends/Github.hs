{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

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
import qualified Data.Map                  as Map
import           Data.Maybe
import           Data.Proxy
import           Data.String               (fromString)
import           Data.String.Conversions   (cs)
import qualified Github.Auth               as GH
import qualified Github.Issues             as GH
import qualified Github.Repos              as GH
import qualified Github.Search             as GH
import           Text.Read
import           Text.RegexPR

data GithubRepoRef = GithubRepoRef String String deriving Eq

instance Show GithubRepoRef where
  show (GithubRepoRef owner repo) = owner ++ "/" ++ repo

newtype GithubIssueId = GithubIssueId { unGithubIssueId :: Int } deriving Eq

instance Show GithubIssueId where
  show (GithubIssueId issueNumber) = "#" ++ show issueNumber

newtype GithubIssueType = GithubIssueType { unGithubIssueType :: String } deriving Eq

instance Show GithubIssueType where
  show = show . unGithubIssueType

instance IsIssueId GithubIssueId where

instance IsIssueType GithubIssueType where
  issueTypeName = unGithubIssueType
  issueTypeDescription _ = "-"

instance IsIssue GH.Issue where
  type IssueId GH.Issue = GithubIssueId
  type IssueType GH.Issue = GithubIssueType
  type IssueTypeIdentifier GH.Issue = String

  issueId = GithubIssueId . GH.issueNumber
  issueStatus = maybe Open (const Closed) . GH.issueClosedAt
  issueType = GithubIssueType . cs . GH.issueState

  summarize issue = unlines'
    [ "Issue Number: " ++ show (GH.issueNumber issue)
    , "ID: " ++ show (GH.issueId issue)
    , "Title: " ++ cs (GH.issueTitle issue)
    , "State: " ++ cs (GH.issueState issue)
    , "Labels: " ++ renderLabels (GH.issueLabels issue)
    , "Assignee: " ++ renderAssignee (GH.issueAssignee issue)
    , "Body:\n" ++ cs (fromMaybe "" (GH.issueBody issue))
    ]
    where
      renderLabels   = intercalate ", " . map (cs . GH.labelName)
      renderAssignee = maybe "-" (cs . GH.untagName . GH.githubOwnerLogin)

  summarizeOneLine issue = show (GH.issueNumber issue) ++ ": " ++ cs (GH.issueTitle issue)
  suggestedBranchName = cs . GH.issueTitle

instance IssueBackend GithubConfig where
  type Issue GithubConfig = GH.Issue
  type IssueCreationData GithubConfig = GH.NewIssue

  createIssue creationData ghConfig = do
    GithubRepoRef owner repo <- issueRepositoryRef ghConfig
    let auth = githubAuth ghConfig
    issue <- liftGithubIO $ GH.createIssue auth (fromString owner) (fromString repo) creationData
    return $ issueId issue

  getIssueById issueId ghConfig = do
    repoRef <- issueRepositoryRef ghConfig
    fetchIssue ghConfig repoRef issueId

  makeIssueTransition (GithubIssueId issueNumber) transition ghConfig = do
    GithubRepoRef owner repo <- issueRepositoryRef ghConfig
    let auth = githubAuth ghConfig
    let edit = GH.EditIssue Nothing Nothing Nothing (Just $ cs transition) Nothing Nothing
    void . liftGithubIO $
      GH.editIssue auth (fromString owner) (fromString repo) (GH.mkId Proxy issueNumber) edit

  close = flip makeIssueTransition "closed"

  reopen = flip makeIssueTransition "open"

  issueUrl issueId ghConfig = do
    issue <- getIssueById issueId ghConfig
    case GH.issueHtmlUrl issue of
      Nothing  -> throwError . IOException $ "Could not get issue URL for issue " ++ show issueId
      Just url -> return $ cs url

  parseIssueId s _ = GithubIssueId <$> readMaybe s `orThrow` ex
    where ex = IOException $ "Unable to parse issue ID: " ++ s

  extractIssueId branch _ = liftMaybe ex $ do
    s <- show branch =~~ "(\\d+)"
    GithubIssueId <$> readMaybe s
    where ex = IOException $ "Unable to extract issue ID from branch: " ++ show branch

  activeIssueId _ =
    return Nothing

  toIssueTypeIdentifier s _ = s

  getAvailableIssueTypes _ = return [GithubIssueType "(Any string)"]

  getIssueTypeAliasMap _ = return Map.empty

  makeIssueCreationData issueType summary _ =
    GH.NewIssue (cs summary) Nothing Nothing Nothing (Just [cs issueType])

  searchIssues options s ghConfig = do
    when optionsSelected $
      liftIO . putStrLn $ "Note: search options are currently ignored when using the Github backend"

    searchString <- issueSearchString s <$> issueRepositoryRef ghConfig
    searchResult <- liftGithubIO $ GH.searchIssues [("q", Just $ cs searchString)]
    return $ GH.searchIssuesIssues searchResult
    where
      optionsSelected = searchOverAllProjects options || searchOnlyUserIssues options

  searchUrl options s ghConfig = do
    when optionsSelected $
      liftIO . putStrLn $ "Note: search options are currently ignored when using the Github backend"

    repoRef@(GithubRepoRef owner repo) <- issueRepositoryRef ghConfig
    let searchString = issueSearchString s repoRef
    return $ "https://github.com/" ++ owner ++ "/" ++ repo ++ "/issues?q=" ++ urlEncode searchString
    where
      optionsSelected = searchOverAllProjects options || searchOnlyUserIssues options

  getSearchAliasMap _ = return Map.empty

  testBackend ghConfig = do
    GithubRepoRef owner repo <- currentRepositoryRef $ Just ghConfig
    liftIO . putStrLn $ "Using repository: " ++ owner ++ "/" ++ repo
    void . liftGithubIO $ GH.repository (fromString owner) (fromString repo)

issueSearchString :: String -> GithubRepoRef -> String
issueSearchString s (GithubRepoRef owner repo) = s ++ " repo:" ++ owner ++ "/" ++ repo

issueRepositoryRef :: GithubConfig -> AppM GithubRepoRef
issueRepositoryRef ghConfig = do
  repoRef <- currentRepositoryRef $ Just ghConfig
  repo <- fetchRepo ghConfig repoRef
  let hasIssues = fromMaybe False $ GH.repoHasIssues repo
  let mParentRepo = forkedFromRepoRef repo

  return $
    case (hasIssues, mParentRepo) of
      (False, Just parentRepo) -> parentRepo
      _                        -> repoRef

instance PullRequestBackend GithubConfig where
  createPullRequestUrl sourceBranch targetBranch ghConfig = do
    repoRef@(GithubRepoRef owner repo') <- currentRepositoryRef $ Just ghConfig
    repo <- fetchRepo ghConfig repoRef

    GithubRepoRef baseOwner _ <- case forkedFromRepoRef repo of
      Nothing            -> return repoRef
      Just parentRepoRef -> do
        -- Ask the user whether to use the parent repo
        useParent <- liftIO . askYesNoWithDefault True $
          "The repository " ++ show repoRef ++ " is forked from " ++ show parentRepoRef ++
          ". Do you want to target the parent repository?"

        return $ if useParent
                 then parentRepoRef
                 else repoRef

    return $ "https://github.com/"
      ++ urlEncode baseOwner
      ++ "/"
      ++ urlEncode repo'
      ++ "/compare/"
      ++ urlEncode (toBranchString targetBranch)
      ++ "..."
      ++ urlEncode owner
      ++ ":"
      ++ urlEncode (toBranchString sourceBranch)

currentRepositoryRef :: Maybe GithubConfig -> AppM GithubRepoRef
currentRepositoryRef mConfig = do
  config <- getConfig
  let remote = config^.configRemoteName
  case mConfig <|> config^.configGithubConfig of
    Nothing       -> refFromRemote remote
    Just ghConfig -> refFromGithubConfig ghConfig <|||> refFromRemote remote
  where
    refFromGithubConfig :: GithubConfig -> AppM GithubRepoRef
    refFromGithubConfig ghConfig =
      let ref = GithubRepoRef <$> ghConfig^.githubUsername <*> ghConfig^.githubRepo
          ex  = ConfigException "Could not read github repo from config"
      in  ref `orThrow` ex

    refFromRemote :: String -> AppM GithubRepoRef
    refFromRemote remote = do
      url <- liftGit $ remoteUrl remote
      extractRepository url `orThrow` extractException url
      where
        extractException url =
          GitException $ "Unable to parse github repository from URL '" ++ url ++ "'"

currentRepositoryRef' :: AppM GithubRepoRef
currentRepositoryRef' = currentRepositoryRef Nothing

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

githubAuth' :: GithubConfig -> Maybe GH.GithubAuth
githubAuth' config = GH.GithubOAuth <$> mtoken
  where
    mtoken = case config^.githubOAuthToken of
      ""    -> Nothing
      token -> Just token

fetchRepo :: GithubConfig -> GithubRepoRef -> AppM GH.Repo
fetchRepo ghConfig (GithubRepoRef user repo) =
  let auth = githubAuth' ghConfig
  in  liftGithubIO $ GH.repository' auth (fromString user) (fromString repo)

fetchIssue :: GithubConfig -> GithubRepoRef -> GithubIssueId -> AppM GH.Issue
fetchIssue ghConfig (GithubRepoRef owner repo) (GithubIssueId issueId) =
  let auth = githubAuth' ghConfig
  in liftGithubIO $ GH.issue' auth (fromString owner) (fromString repo) (GH.mkId Proxy issueId)

liftGithub :: Either GH.Error a -> Either AppException a
liftGithub = mapLeft (IOException . show)

liftGithubIO :: IO (Either GH.Error a) -> AppM a
liftGithubIO = liftEitherIO . fmap liftGithub

fromRepoRef :: GH.RepoRef -> GithubRepoRef
fromRepoRef (GH.RepoRef owner repo) = GithubRepoRef (GH.untagName $ GH.githubOwnerLogin owner) $ GH.untagName repo
