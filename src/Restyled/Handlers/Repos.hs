module Restyled.Handlers.Repos
    ( getRepoR
    , putRepoR
    , getRepoJobLogLinesR
    ) where

import Restyled.Prelude

import Conduit
import Control.Monad.Validate
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Restyled.Api.UpsertRepo
import Restyled.Foundation
import Restyled.Handlers.Repos.Jobs
import Restyled.JobOutput
import Restyled.Models hiding (upsertRepo)
import Restyled.WebSockets
import Restyled.Widgets.Job
import Restyled.Widgets.JobLogLine
import Restyled.Yesod

getRepoR :: OwnerName -> RepoName -> Handler Html
getRepoR = getRepoJobsR

-- | Find or create a repository
--
-- - Request: 'ApiUpsertRepo'
-- - Response: 400 with 'ApiUpsertRepoErrors' or 200 with 'ApiRepo'
--
putRepoR :: OwnerName -> RepoName -> Handler Value
putRepoR owner name = do
    body <- requireJsonBody
    result <- runDB $ runValidateT $ do
        assertOwnerName owner body *> assertRepoName name body
        upsertRepo body
    either (sendStatusJSON status400) (sendStatusJSON status200) result


getRepoJobLogLinesR :: OwnerName -> RepoName -> JobId -> Handler Text
getRepoJobLogLinesR _owner _name jobId = do
    job <- runDB $ getEntity404 jobId

    webSockets $ ignoringConnectionException $ do
        conn <- ask

        let conduit :: ConduitT [JobLogLine] [JobLogLine] Handler ()
            conduit = mapMC $ \jobLogLines -> do
                t <- formatJobLogLines jobLogLines
                jobLogLines <$ runReaderT (sendTextDataAck t) conn

        lift $ followJobOutput job conduit

    -- If not accessed via WebSockets, respond with plain text Job log
    jobLogLines <- fetchJobOutput job
    pure $ T.unlines $ map textJobLogLine jobLogLines

formatJobLogLines :: [JobLogLine] -> Handler Text
formatJobLogLines jobLogLines = do
    htmls <- traverse renderJobLogLine jobLogLines
    pure $ TL.toStrict $ mconcat htmls
