module Main where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import System.IO (hFlush, stdout)
import qualified Network.HTTP.Req as H
import Network.HTTP.Req ((/:), Req)
import Data.Aeson (Value, (.:), (.=))
import Data.Aeson.Types (parseMaybe, Parser)
import qualified Data.Aeson as A
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe (fromJust)

data RequestDetails = RequestDetails
  { competitionID :: Text
  , userAgent :: ByteString
  , verificationCode :: Text
  }

data Participant = Participant
  { username :: Text
  , accountType :: Text
  }

promptText :: String -> IO Text
promptText p = do
  putStr p
  hFlush stdout
  TIO.getLine

promptBS :: String -> IO ByteString
promptBS p = do
  putStr p
  hFlush stdout
  BS.getLine

promptDetails :: IO RequestDetails
promptDetails = do
  compid <- promptText "Enter the competition ID: "
  useragent <- promptBS "Enter the user agent (your discord username): "
  vercode <- promptText "Enter the WOM verification code: "
  pure $ RequestDetails
    { competitionID = compid
    , userAgent = useragent
    , verificationCode = vercode
    }

getCompetition :: RequestDetails -> Req Value
getCompetition details = H.responseBody <$> H.req
  H.GET
  (H.https "api.wiseoldman.net" /:
    "v2" /:
    "competitions" /:
    competitionID details)
  H.NoReqBody
  H.jsonResponse
  (H.header "user-agent" $ userAgent details)

-- Get everyone participating
getParticipation :: Value -> Vector Participant
getParticipation = fromJust . parseMaybe parser where
  parser =
    A.withObject "comp" $ \comp ->
    comp .: "participations" >>=
    A.withArray "participations" (traverse getParticipant)

getParticipant :: Value -> Parser Participant
getParticipant =
  A.withObject "Participation" $ \participation ->
  participation .: "player" >>=
  A.withObject "Player" (\player -> do
    name <- player .: "username"
    acctype <- player .: "type"
    pure $ Participant
      { username = name
      , accountType = acctype
      }
    )

justIrons :: Vector Participant -> Vector Participant
justIrons = V.filter isIron

isIron :: Participant -> Bool
isIron p = case accountType p of
  "ultimate" -> True
  "hardcore" -> True
  "ironman" -> True
  _ -> False

updateParticipants :: RequestDetails -> Vector Text -> Req ()
updateParticipants details participants = () <$ H.req
  H.PUT
  (H.https "api.wiseoldman.net" /:
    "v2" /:
    "competitions" /:
    competitionID details)
  (H.ReqBodyJson $ A.object
    [ "participants" .= participants
    , "verificationCode" .= verificationCode details
    ] )
  H.ignoreResponse
  (H.header "user-agent" $ userAgent details)

main :: IO ()
main = do
  -- Get all details needed
  details <- promptDetails

  H.runReq H.defaultHttpConfig $ do
    -- Fetch the comp
    comp <- getCompetition details
    -- Determine the new player list
    let playerList = username <$> justIrons (getParticipation comp)
    -- Post the player list
    updateParticipants details playerList
