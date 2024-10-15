module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import System.IO (hFlush, stdout)
import qualified Network.HTTP.Req as H
import Network.HTTP.Req ((/:), Req)
import Data.Aeson (Value, (.:), (.=))
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe (fromJust)
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity (Identity(..))
import Control.Applicative ((<**>))
import qualified Options.Applicative as O
import Data.Foldable (fold)

data RequestDetailsT f = RequestDetailsT
  { competitionID :: f Text
  , discordUser :: f ByteString
  , verificationCode :: f Text
  }

type RequestDetails = RequestDetailsT Identity

userAgent :: RequestDetails -> ByteString
userAgent details = fold
  [ "discord: "
  , runIdentity $ discordUser details
  , "; script: IronCompetition"
  ]

data Participant = Participant
  { username :: Text
  , accountType :: Text
  }

-- Get command line arguments
getArgs :: O.ParserInfo (RequestDetailsT Maybe)
getArgs = O.info
  (parser <**> O.helper)
  $ fold
    [ O.header "Iron Competition"
    , O.progDesc $ fold
      [ "Removes all non-ironman participants from a competition on "
      , "Wise Old Man. All command line options are optional, though missing "
      , "values will be prompted for when the program runs."
      ]
    ]
  where
  parser = RequestDetailsT <$> compIDParse <*> discordParse <*> vercodeParse
  compIDParse = O.optional $ O.strOption $ fold
    [ O.long "competition"
    , O.short 'c'
    , O.metavar "ID"
    , O.help "Competition ID from Wise Old Man"
    ]
  discordParse = O.optional $ O.strOption $ fold
    [ O.long "discord"
    , O.short 'd'
    , O.metavar "NAME"
    , O.help "Discord username to be used in user agent"
    ]
  vercodeParse = O.optional $ O.strOption $ fold
    [ O.long "verification"
    , O.short 'v'
    , O.metavar "CODE"
    , O.help "Verification code for the competition"
    ]

promptText :: String -> IO Text
promptText p = do
  putStr p
  hFlush stdout
  T.strip <$> TIO.getLine

promptBS :: String -> IO ByteString
promptBS p = do
  putStr p
  hFlush stdout
  BS.strip <$> BS.getLine

promptDetails :: RequestDetailsT Maybe -> IO RequestDetails
promptDetails argDeets = do
  compid <- case competitionID argDeets of
    Just c -> pure c
    Nothing -> promptText "Enter the competition ID: "
  discord <- case discordUser argDeets of
    Just d -> pure d
    Nothing -> promptBS "Enter your discord username: "
  vercode <- case verificationCode argDeets of
    Just v -> pure v
    Nothing -> promptText "Enter the competition verification code: "
  pure $ RequestDetailsT
    { competitionID = Identity compid
    , discordUser = Identity discord
    , verificationCode = Identity vercode
    }

getCompetition :: RequestDetails -> Req Value
getCompetition details = H.responseBody <$> H.req
  H.GET
  (H.https "api.wiseoldman.net" /:
    "v2" /:
    "competitions" /:
    runIdentity (competitionID details))
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

getParticipant :: Value -> AT.Parser Participant
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

nonIrons :: Vector Participant -> Vector Participant
nonIrons = V.filter (not . isIron)

isIron :: Participant -> Bool
isIron p = case accountType p of
  "ultimate" -> True
  "hardcore" -> True
  "ironman" -> True
  _ -> False

removeParticipants :: RequestDetails -> Vector Text -> Req Text
removeParticipants details participants
  | V.null participants = pure "No one to remove"
  | otherwise = do
    responseValue <- H.responseBody <$> H.req
      H.DELETE
      (H.https "api.wiseoldman.net" /:
        "v2" /:
        "competitions" /:
        runIdentity (competitionID details) /:
        "participants")
      (H.ReqBodyJson $ A.object
        [ "participants" .= participants
        , "verificationCode" .= runIdentity (verificationCode details)
        ] )
      H.jsonResponse
      (H.header "user-agent" $ userAgent details)
    pure $ fromJust $ flip parseMaybe responseValue $ A.withObject
      "Response"
      (.: "message")

main :: IO ()
main = do
  -- Get command line arguments
  args <- O.execParser getArgs
  -- Get all remaining details needed
  details <- promptDetails args
  -- Do some API requests
  H.runReq H.defaultHttpConfig $ do
    -- Fetch the comp
    comp <- getCompetition details
    -- Determine the players to remove
    let playerList = username <$> nonIrons (getParticipation comp)
    -- Delete the removed player list
    result <- removeParticipants details playerList
    -- Print the result for the user
    liftIO $ TIO.putStrLn result
