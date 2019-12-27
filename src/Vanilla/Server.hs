{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Vanilla.Server where

import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.List                      ( sort )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Data.Text.Lazy                as LT
import           Data.Text.Lazy.Encoding        ( encodeUtf8 )
import           GHC.Generics                   ( Generic )
import           Servant
import           System.Directory               ( doesFileExist
                                                , listDirectory
                                                )
import           System.FilePath
import           Vanilla                        ( runVanilla )

newtype RunReq = RunReq {program :: Text} deriving (Eq, Show, Generic)

data RunRes = RunRes {val :: Text, ty :: Text} deriving (Eq, Show, Generic)

newtype ExampleReq = ExampleReq {example :: Text} deriving (Eq, Show, Generic)

newtype ExampleRes = ExampleRes {program :: Text} deriving (Eq, Show, Generic)

instance FromJSON RunReq

instance ToJSON RunRes

instance FromJSON ExampleReq

instance ToJSON ExampleRes

-- brittany-disable-next-binding
type API
  = "run" :> ReqBody '[JSON] RunReq :> Get '[JSON] RunRes
    :<|> ("example" :>
            ("list" :> Get '[JSON] [Text]
            :<|> ReqBody '[JSON] ExampleReq :> Get '[JSON] ExampleRes))


server :: Server API
server = run :<|> (exampleList :<|> fetchExample)
 where
  run :: RunReq -> Handler RunRes
  run (RunReq prog) = case runVanilla "program.vn" prog of
    Left e -> throwError $ err400 { errBody = encodeUtf8 . LT.pack $ e }
    Right (val, ty) -> return $ RunRes (T.pack $ show val) (T.pack $ show ty)
  fetchExample :: ExampleReq -> Handler ExampleRes
  fetchExample (ExampleReq example) = do
    let exampleFilePath = "example" </> T.unpack example <.> "vn"
    exist <- liftIO $ doesFileExist exampleFilePath
    if exist
      then liftIO . fmap ExampleRes $ TIO.readFile exampleFilePath
      else throwError err404 { errBody = "Example doesn't exist" }
  exampleList :: Handler [Text]
  exampleList = do
    examples <- liftIO $ filter (isExtensionOf "vn") <$> listDirectory "example"
    return . sort $ T.pack . dropExtension <$> examples

api :: Proxy API
api = Proxy

app = serve api server
