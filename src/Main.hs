{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((&), (.~), (<&>), (?~))
import Data.Text (Text)
import Network.Google
import Network.Google.Storage
import System.IO (stdout)

import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise
import Text.Show.Pretty (ppShow)

import qualified Data.Text as T
import qualified System.Environment as Env

pprint :: Show a => a -> IO ()
pprint =
  putStrLn . hscolour TTY defaultColourPrefs False False "" False . ppShow

uploadIt :: Text -> FilePath -> IO Object
uploadIt bkt filename = do
  lgr <- newLogger Debug stdout -- (1)
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ storageReadWriteScope) -- (2) (3)
  body <- sourceBody filename -- (4)
  let key = T.pack filename
  runResourceT . runGoogle env $ -- (5)
    upload (objectsInsert bkt object' & oiName ?~ key) body

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [bkt, filename] -> do
      putStrLn "Uploading..."
      object <- uploadIt (T.pack bkt) filename
      pprint ("the object" :: String, object)
    _ -> putStrLn "usage: gcs-upload bucket-name filename"
