{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((&), (.~), (<&>), (?~))
import Data.Text (Text)
import Network.Google
import Network.Google.Storage
import System.IO (stdout)

import qualified Data.Text as T
import qualified System.Environment as Env

uploadIt :: Text -> FilePath -> IO Object
uploadIt bucket filename = do
  lgr <- newLogger Debug stdout -- (1)
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ storageReadWriteScope) -- (2) (3)
  body <- sourceBody filename -- (4)
  let key = T.pack filename
      bkt = bucket
  runResourceT . runGoogle env $ -- (5)
    upload (objectsInsert bkt object' & oiName ?~ key) body

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [bucket, filename] -> do
      putStrLn "Uploading..."
      object <- uploadIt (T.pack bucket) filename
      print ("the object", object)
