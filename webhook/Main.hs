{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Postlude

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import GitHub.App.Auth (AppAuth (AppAuth), InstallationAuth, mkInstallationAuth)
import GitHub.App.Request (executeAppRequest)
import GitHub.Data (Permission (PermissionAdmin), Team)
import GitHub.Data.Id (Id (Id))
import GitHub.Data.Name (Name (N))
import GitHub.Data.Installations (Installation)
import GitHub.Data.Webhooks.Events (InstallationEvent (..), InstallationRepositoriesEvent (..), InstallationEventAction (..))
import GitHub.Data.Webhooks.Payload (HookRepositorySimple (..), HookInstallation (..))
import GitHub.Endpoints.Organizations.Teams (addOrUpdateTeamRepoR)
import Network.Wai.Handler.Warp (runEnv)
import Servant.API ((:>), JSON, Post)
import Servant.Generic ((:-), AsApi, AsServerT, ToServant, toServant)
import Servant.GitHub.Webhook (GitHubEvent, GitHubSignedReqBody, RepoWebhookEvent (WebhookInstallationEvent, WebhookInstallationRepositoriesEvent))
import Crypto.PubKey.RSA.Read (readRsaPem)
import qualified Servant.GitHub.Webhook (GitHubKey, gitHubKey)
import Servant.Server (Context ((:.), EmptyContext), Handler (Handler),
                       HasContextEntry (getContextEntry), hoistServerWithContext, serveWithContext)
import System.Environment (lookupEnv)


teamId :: Id Team
teamId = Id 2840739

type AppHandler = ReaderT AppAuth (StateT (Map (Id Installation) InstallationAuth) IO)

usingAppHandler :: AppAuth -> AppHandler a -> Handler a
usingAppHandler auth = Handler . lift . flip evalStateT mempty . flip runReaderT auth


data OpsAccess route = OpsAccess
    { _oaInstallation :: route
        :- GitHubEvent '[ 'WebhookInstallationEvent ]
        :> GitHubSignedReqBody '[JSON] InstallationEvent
        :> Post '[JSON] ()
    , _oaInstallationRepositories :: route
        :- GitHubEvent '[ 'WebhookInstallationRepositoriesEvent ]
        :> GitHubSignedReqBody '[JSON] InstallationRepositoriesEvent
        :> Post '[JSON] ()
    }
    deriving Generic

opsAccessServer :: OpsAccess (AsServerT AppHandler)
opsAccessServer = OpsAccess {..}
  where
    _oaInstallation :: RepoWebhookEvent -> ((), InstallationEvent) -> AppHandler ()
    _oaInstallation _ ((), ev) = do
      liftIO $ putStrLn "yo"
      case evInstallationAction ev of
        InstallationCreatedAction -> do
            liftIO $ putStrLn "Installation created action started"
            instAuth <- registerInstallation instId
            liftIO $ putStrLn "Handling repos"
            liftIO $ forM_ (evInstallationRepos ev) (handleRepo instAuth)
            liftIO $ putStrLn "Done"
        InstallationDeletedAction -> modify $ M.delete instId
        InstallationActionOther _ -> error "Unknown action"
      where
        instId = Id . whInstallationId . evInstallationInfo $ ev

    _oaInstallationRepositories :: RepoWebhookEvent -> ((), InstallationRepositoriesEvent) -> AppHandler ()
    _oaInstallationRepositories _ ((), ev) = do
        mInstAuth <- gets $ M.lookup instId
        instAuth <- case mInstAuth of
            Nothing -> registerInstallation instId
            Just instAuth -> pure instAuth
        liftIO $ forM_ (evInstallationReposAdd ev) (handleRepo instAuth)
      where
        instId = Id . whInstallationId . evInstallationRepoInfo $ ev

    registerInstallation :: Id Installation -> AppHandler InstallationAuth
    registerInstallation instId = do
        appAuth <- ask
        instAuth <- liftIO $ mkInstallationAuth appAuth instId
        modify $ M.insert instId instAuth
        pure instAuth

    handleRepo :: InstallationAuth -> HookRepositorySimple -> IO ()
    handleRepo auth HookRepositorySimple{whSimplRepoFullName = repoFullName} = do
        let [orgName, repoName] = T.splitOn "/" repoFullName
        putStrLn $ "Processing " <> T.unpack orgName <> "/" <> T.unpack repoName
        res <- executeAppRequest auth (addOrUpdateTeamRepoR teamId (N orgName) (N repoName) PermissionAdmin)
        case res of
            Left err -> putStrLn $ "Error: " <> show err
            Right () -> pure ()


main :: IO ()
main = do
    secret <- maybe "" C8.pack <$> lookupEnv "GITHUB_WEBHOOK_SECRET"
    appId <- maybe (error "missing") (Id . read) <$> lookupEnv "GITHUB_APP_ID"
    appPkPem <- maybe (error "missing") C8.pack <$> lookupEnv "GITHUB_APP_PK"
    let Right appPk = readRsaPem appPkPem
        auth = AppAuth appId appPk

    let api = Proxy :: Proxy (ToServant (OpsAccess AsApi))
        ctx = Proxy :: Proxy '[ GitHubKey ]
        app = serveWithContext api (gitHubKey (pure secret) :. EmptyContext)
            . hoistServerWithContext api ctx (usingAppHandler auth)
            $ toServant opsAccessServer
    runEnv 8123 app


---
-- HACK
---

newtype GitHubKey = GitHubKey (forall result. Servant.GitHub.Webhook.GitHubKey {-result-})

gitHubKey :: IO ByteString -> GitHubKey
gitHubKey k = GitHubKey (Servant.GitHub.Webhook.gitHubKey k)

instance HasContextEntry '[GitHubKey] (Servant.GitHub.Webhook.GitHubKey {-result-}) where
    getContextEntry (GitHubKey x :. _) = x
