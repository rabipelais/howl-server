{-# LANGUAGE OverloadedStrings
           , Rank2Types
           , ScopedTypeVariables
           , GADTs
           , FlexibleContexts #-}
module FacebookTests (facebookSpecTests, getCredentials) where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Function (on)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Time (parseTime)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import System.Environment (getEnv)
import System.Exit (exitFailure)
import System.IO.Error (isDoesNotExistError)


import qualified Control.Exception.Lifted as E
import qualified Control.Monad.Trans.Resource as R
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Char8 as B
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Default as D
import qualified Data.Map as Map
import qualified Data.Maybe as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time as TI
import qualified Howl.Facebook as FB
import qualified Network.HTTP.Conduit as H
import qualified Test.QuickCheck as QC


import Test.HUnit ((@?=))
import Test.Hspec
import Test.Hspec.HUnit ()
import Test.Hspec.QuickCheck
import Test.Tasty
import Test.Tasty.Hspec


-- | Grab the Facebook credentials from the environment.
getCredentials :: IO FB.Credentials
getCredentials = tryToGet `E.catch` showHelp
    where
      tryToGet = do
        [appName, appId, appSecret] <- mapM getEnv ["APP_NAME", "APP_ID", "APP_SECRET"]
        return $ FB.Credentials (T.pack appName) (T.pack appId) (T.pack appSecret)

      showHelp exc | not (isDoesNotExistError exc) = E.throw exc
      showHelp _ = do
        putStrLn $ unlines
          [ "In order to run the tests from the 'fb' package, you need"
          , "developer access to a Facebook app.  The tests are designed"
          , "so that your app isn't going to be hurt, but we may not"
          , "create a Facebook app for this purpose and then distribute"
          , "its secret keys in the open."
          , ""
          , "Please give your app's name, id and secret on the enviroment"
          , "variables APP_NAME, APP_ID and APP_SECRET, respectively.  "
          , "For example, before running the test you could run in the shell:"
          , ""
          , "  $ export APP_NAME=\"example\""
          , "  $ export APP_ID=\"458798571203498\""
          , "  $ export APP_SECRET=\"28a9d0fa4272a14a9287f423f90a48f2304\""
          , ""
          , "Of course, these values above aren't valid and you need to"
          , "replace them with your own."
          , ""
          , "(Exiting now with a failure code.)"]
        exitFailure


invalidCredentials :: FB.Credentials
invalidCredentials = FB.Credentials "this" "isn't" "valid"

invalidUserAccessToken :: FB.UserAccessToken
invalidUserAccessToken = FB.UserAccessToken (FB.Id "invalid") "user" farInTheFuture
    where
      Just farInTheFuture = parseTime (error "farInTheFuture") "%Y" "3000"
      -- It's actually important to use 'farInTheFuture' since we
      -- don't want any tests rejecting this invalid user access
      -- token before even giving it to Facebook.

invalidAppAccessToken :: FB.AppAccessToken
invalidAppAccessToken = FB.AppAccessToken "invalid"


facebookSpecTests :: IO TestTree
facebookSpecTests = H.withManager $ \manager -> liftIO $ do
  creds <- getCredentials
  testSpec "Facebook tests" $ do
    -- Run the tests twice, once in Facebook's production tier...
    facebookTests "Production tier: "
                  creds
                  manager
                  (R.runResourceT . FB.runFacebookT creds manager)
                  (R.runResourceT . FB.runNoAuthFacebookT manager)
    -- ...and the other in Facebook's beta tier.
    --facebookTests "Beta tier: "
    --              creds
    --              manager
    --              (R.runResourceT . FB.beta_runFacebookT creds manager)
    --              (R.runResourceT . FB.beta_runNoAuthFacebookT manager)

    -- Tests that don't depend on which tier is chosen.
    libraryTests manager

facebookTests :: String
              -> FB.Credentials
              -> H.Manager
              -> (forall a. FB.FacebookT FB.Auth   (R.ResourceT IO) a -> IO a)
              -> (forall a. FB.FacebookT FB.NoAuth (R.ResourceT IO) a -> IO a)
              -> Spec
facebookTests pretitle creds manager runAuth runNoAuth = do
  let describe' = describe . (pretitle ++)

  describe' "getAppAccessToken" $ do
    it "works and returns a valid app access token" $
      runAuth $ do
        token <- FB.getAppAccessToken
        FB.isValid token #?= True
    it "throws a FacebookException on invalid credentials" $
      R.runResourceT $
      FB.runFacebookT invalidCredentials manager $ do
        ret <- E.try $ FB.getAppAccessToken
        case ret  of
          Right token                      -> fail $ show token
          Left (_ :: FB.FacebookException) -> lift $ lift (return () :: IO ())

  describe' "isValid" $ do
    it "returns False on a clearly invalid user access token" $
      runNoAuth $ FB.isValid invalidUserAccessToken #?= False
    it "returns False on a clearly invalid app access token" $
      runNoAuth $ FB.isValid invalidAppAccessToken  #?= False

  describe' "debugToken" $ do
    it "works on a test user access token" $ do
      runAuth $
        withTestUser D.def $ \testUser -> do
          Just testUserAccessTokenData <- return (FB.tuAccessToken testUser)
          appToken <- FB.getAppAccessToken
          ret <- FB.debugToken appToken testUserAccessTokenData
          now <- liftIO TI.getCurrentTime
          FB.dtAppId ret &?= Just (FB.appId creds)
          FB.dtAppName ret &?= Just (FB.appName creds)
          case FB.dtExpiresAt ret of
            Nothing -> fail "dtExpiresAt is Nothing"
            Just t  -> compare t now &?= GT
          FB.dtIsValid ret &?= Just True
          case FB.dtIssuedAt ret of
            Nothing -> return () -- ok since it's a test user
            Just t  -> compare t now &?= LT
          isJust (FB.dtScopes ret) &?= True
          FB.dtUserId ret &?= Just (FB.tuId testUser)
          case FB.dtAccessToken ret of
            Nothing -> fail "dtAccessToken is Nothing"
            Just t -> do
              let f :: FB.UserAccessToken -> FB.FacebookT FB.Auth (R.ResourceT IO) ()
                  f (FB.UserAccessToken uid dt exps) = do
                    uid &?= FB.tuId testUser
                    dt  &?= testUserAccessTokenData
                    Just exps &?= FB.dtExpiresAt ret
              f t

  describe' "getObject" $ do
    it "is able to fetch Facebook's own page" $
      runAuth $ do
        appToken <- FB.getAppAccessToken
        A.Object obj <- FB.getObject "/19292868552" [("fields", "id,name,website")] (Just appToken)
        let Just r = flip A.parseMaybe () $ const $
                     (,,) <$> obj A..:? "id"
                          <*> obj A..:? "website"
                          <*> obj A..:? "name"
            just x = Just (x :: Text)
        r &?= ( just "19292868552"
              , just "http://developers.facebook.com"
              , just "Facebook for Developers" )


newtype PageName = PageName Text deriving (Eq, Show)
instance A.FromJSON PageName where
  parseJSON (A.Object v) = PageName <$> (v A..: "name")
  parseJSON _ = mzero


libraryTests :: H.Manager -> Spec
libraryTests manager = do
  describe "SimpleType" $ do
    it "works for Bool" $ (map FB.encodeFbParam [True, False]) @?= ["1", "0"]

    let day       = TI.fromGregorian 2012 12 21
        time      = TI.TimeOfDay 11 37 22
        diffTime  = TI.secondsToDiffTime (11*3600 + 37*60)
        utcTime   = TI.UTCTime day diffTime
        localTime = TI.LocalTime day time
        zonedTime = TI.ZonedTime localTime (TI.minutesToTimeZone 30)
    it "works for Day"       $ FB.encodeFbParam day       @?= "2012-12-21"
    it "works for UTCTime"   $ FB.encodeFbParam utcTime   @?= "20121221T1137Z"
    it "works for ZonedTime" $ FB.encodeFbParam zonedTime @?= "20121221T1107Z"

    let propShowRead :: (Show a, Read a, Eq a, FB.SimpleType a) => a -> Bool
        propShowRead x = read (B.unpack $ FB.encodeFbParam x) == x
    prop "works for Float"  (propShowRead :: Float  -> Bool)
    prop "works for Double" (propShowRead :: Double -> Bool)
    prop "works for Int"    (propShowRead :: Int    -> Bool)
    prop "works for Int8"   (propShowRead :: Int8   -> Bool)
    prop "works for Int16"  (propShowRead :: Int16  -> Bool)
    prop "works for Int32"  (propShowRead :: Int32  -> Bool)
    prop "works for Int64"  (propShowRead :: Int64  -> Bool)
    prop "works for Word"   (propShowRead :: Word   -> Bool)
    prop "works for Word8"  (propShowRead :: Word8  -> Bool)
    prop "works for Word16" (propShowRead :: Word16 -> Bool)
    prop "works for Word32" (propShowRead :: Word32 -> Bool)
    prop "works for Word64" (propShowRead :: Word64 -> Bool)

    let propShowReadL :: (Show a, Read a, Eq a, FB.SimpleType a) => [a] -> Bool
        propShowReadL x = read ('[' : B.unpack (FB.encodeFbParam x) ++ "]") == x
    prop "works for [Float]"  (propShowReadL :: [Float]  -> Bool)
    prop "works for [Double]" (propShowReadL :: [Double] -> Bool)
    prop "works for [Int]"    (propShowReadL :: [Int]    -> Bool)
    prop "works for [Int8]"   (propShowReadL :: [Int8]   -> Bool)
    prop "works for [Int16]"  (propShowReadL :: [Int16]  -> Bool)
    prop "works for [Int32]"  (propShowReadL :: [Int32]  -> Bool)
    prop "works for [Int64]"  (propShowReadL :: [Int64]  -> Bool)
    prop "works for [Word]"   (propShowReadL :: [Word]   -> Bool)
    prop "works for [Word8]"  (propShowReadL :: [Word8]  -> Bool)
    prop "works for [Word16]" (propShowReadL :: [Word16] -> Bool)
    prop "works for [Word32]" (propShowReadL :: [Word32] -> Bool)
    prop "works for [Word64]" (propShowReadL :: [Word64] -> Bool)

    prop "works for Text" (\t -> FB.encodeFbParam t == TE.encodeUtf8 t)

    prop "works for Id" $ \i ->
      let toId :: Int -> FB.Id
          toId = FB.Id . T.pack . show
          j = abs i
      in FB.encodeFbParam (toId j) == FB.encodeFbParam j

  describe "parseSignedRequest" $ do
    let exampleSig, exampleData :: B.ByteString
        exampleSig  = "vlXgu64BQGFSQrY0ZcJBZASMvYvTHu9GQ0YM9rjPSso"
        exampleData = "eyJhbGdvcml0aG0iOiJITUFDLVNIQTI1NiIsIjAiOiJwYXlsb2FkIn0"
        exampleCreds = FB.Credentials "name" "id" "secret"
        runExampleAuth :: FB.FacebookT FB.Auth (R.ResourceT IO) a -> IO a
        runExampleAuth = R.runResourceT . FB.runFacebookT exampleCreds manager
    it "works for Facebook example" $ do
      runExampleAuth $ do
        ret <- FB.parseSignedRequest (B.concat [exampleSig, ".", exampleData])
        ret &?= Just (A.object [ "algorithm" A..= ("HMAC-SHA256" :: Text)
                               , "0"         A..= ("payload"     :: Text)])
    it "fails to parse the Facebook example when signature is corrupted" $ do
      let corruptedSig = B.cons 'a' (B.tail exampleSig)
      runExampleAuth $ do
        ret <- FB.parseSignedRequest (B.concat [corruptedSig, ".", exampleData])
        ret &?= (Nothing :: Maybe A.Value)

  describe "FQLTime" $ do
    it "seems to work" $ do
      let input  = "[1348678357]"
          output = FB.FQLTime (read "2012-09-26 16:52:37 UTC")
      A.decode input @?= Just [output]

  describe "FbUTCTime" $ do
    let output = FB.FbUTCTime (read "2012-09-26 16:52:37 UTC")
    it "seems to work (string)" $ do
      let input  = "[\"2012-09-26T16:52:37+0000\"]"
      A.decode input @?= Just [output]
    it "seems to work (unix epoch)" $ do
      let input  = "[1348678357]"
      A.decode input @?= Just [output]

  describe "FQLList" $ do
    let j :: [Int] -> Maybe (FB.FQLList Int)
        j = Just . FB.FQLList
    it "parses []" $ do
      A.decode "[]" @?= j []
    it "parses {}" $ do
      A.decode "{}" @?= j []
    it "parses [1234]" $ do
      A.decode "[1234]" @?= j [1234]
    it "parses {\"1234\": 1234}" $ do
      A.decode "{\"1234\": 1234}" @?= j [1234]

  describe "FQLObject" $ do
    let j :: [(Text, Int)] -> Maybe (FB.FQLObject (Map.Map Text Int))
        j = Just . FB.FQLObject . Map.fromList
    it "parses []" $ do
      A.decode "[]" @?= j []
    it "parses {}" $ do
      A.decode "{}" @?= j []
    it "parses {\"abc\": 1234}" $ do
      A.decode "{\"abc\": 1234}" @?= j [("abc", 1234)]
    it "does not parse [1234]" $ do
      A.decode "[1234]" @?= (Nothing `asTypeOf` j [])

  describe "Id" $ do
    it "can be parsed from a string" $ do
      A.decode "[\"1234\"]" @?= Just [FB.Id "1234"]
    it "can be parsed from an integer" $ do
      A.decode "[1234]" @?= Just [FB.Id "1234"]
    it "can be parsed from an object with a string" $ do
      A.decode "{\"id\": \"1234\"}" @?= Just (FB.Id "1234")
    it "can be parsed from an object with an integer" $ do
      A.decode "{\"id\": 1234}" @?= Just (FB.Id "1234")

  describe "AccessToken" $ do
    it "can be round-tripped with ToJSON/FromJSON (UserKind)" $ do
      A.eitherDecode (A.encode invalidUserAccessToken) @?= Right invalidUserAccessToken
    it "can be round-tripped with ToJSON/FromJSON (AppKind)" $ do
      A.eitherDecode (A.encode invalidAppAccessToken) @?= Right invalidAppAccessToken


-- Wrappers for HUnit operators using MonadIO

(&?=) :: (Eq a, Show a, MonadIO m) => a -> a -> m ()
v &?= e = liftIO (v @?= e)

(#?=) :: (Eq a, Show a, MonadIO m) => m a -> a -> m ()
m #?= e = m >>= (&?= e)


-- | Sad, orphan instance.
instance QC.Arbitrary Text where
    arbitrary = T.pack <$> QC.arbitrary
    shrink    = map T.pack . QC.shrink . T.unpack


-- | Perform an action with a new test user. Remove the new test user
-- after the action is performed.
withTestUser :: (R.MonadResource m, MonadBaseControl IO m)
                => FB.CreateTestUser
                -> (FB.TestUser -> FB.FacebookT FB.Auth m a)
                -> FB.FacebookT FB.Auth m a
withTestUser ctu action = do
  token <- FB.getAppAccessToken
  E.bracket (FB.createTestUser ctu token)
            (flip FB.removeTestUser token)
            action

getTestToken testUser = do
  appToken <- FB.getAppAccessToken
  Just testUserAccessTokenData <- return (FB.tuAccessToken testUser)
  ret <- FB.debugToken appToken testUserAccessTokenData
  return $ FB.dtAccessToken ret
