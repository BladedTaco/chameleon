{-# LANGUAGE OverloadedStrings #-}

module Main where
import Web.Scotty
    ( json,
      body,
      post,
      scotty,
      ScottyM,
      file,
      get,
      param,
      regex,
      setHeader,
      liftAndCatchIO,
      ScottyM,
      body,
      file,
      get,
      json,
      param,
      post,
      regex,
      scotty,
      setHeader )
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Monoid (mconcat)
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as T
import qualified Wrasse.Hook as Wrasse hiding (main)
import qualified Wrasse.Messages as WrasseMsg

import JsonInstance
import Run hiding (main)
import System.Directory

main = scotty 5000 (
    ghc >> messages >> ace
    >> typecheck
    >> home
    >> sourceMap
    >> js
    >> css
    >> svg
    >> jpg
    >> png
    >> favicon
    >> page
    )


--------------------------------------------
ghc :: ScottyM ()
ghc = post "/ghc" $ do
    content <- body
    result <- liftAndCatchIO $ Wrasse.hook (BS.unpack content)
    json result


messages :: ScottyM ()
messages = post "/messages" $ do
  content <- body
  result <- liftAndCatchIO $ WrasseMsg.messageHook (BS.unpack content)
  json result

ace :: ScottyM ()
ace = post "/ace" $ do
  content <- body
  result <- liftAndCatchIO $ getDirectoryContents =<< getCurrentDirectory 
  json result
----------------------------------------------


typecheck :: ScottyM ()
typecheck = post "/typecheck" $ do
  content <- body
  let result = processFile (BS.unpack content)
  json result

home :: ScottyM ()
home = get "/" $ do
  file "static/build/index.html"

js :: ScottyM ()
js = get (regex "^.*\\.js$") $ do
  path <- param "0"
  let filename = "static/build" `T.append` path
  setHeader "Content-Type" "application/javascript"
  file (T.unpack filename)
css :: ScottyM ()
css = get (regex "^.*\\.css") $ do
  path <- param "0"
  let filename = "static/build" `T.append` path
  setHeader "Content-Type" "text/css"
  file (T.unpack filename)


svg :: ScottyM ()
svg = get (regex "^.*\\.svg") $ do
  path <- param "0"
  let filename = "static/build" `T.append` path
  setHeader "Content-Type" "image/svg+xml"
  file (T.unpack filename)

jpg :: ScottyM ()
jpg = get (regex "^.*\\.jpg") $ do
  path <- param "0"
  let filename = "static/build" `T.append` path
  setHeader "Content-Type" "image/jpeg"
  file (T.unpack filename)

png :: ScottyM ()
png = get (regex "^.*\\.png") $ do
  path <- param "0"
  let filename = "static/build" `T.append` path
  setHeader "Content-Type" "image/png"
  file (T.unpack filename)

favicon :: ScottyM ()
favicon = get "/favicon.ico" $ do
  setHeader "Content-Type" "image/vnd.microsoft.icon"
  file "static/build/favicon.ico"

sourceMap :: ScottyM ()
sourceMap = get (regex "^.*\\.js\\.map")  $ do
  path <- param "0"
  let filename = "static/build" `T.append` path
  setHeader "Content-Type" "application/json"
  file (T.unpack filename)

page :: ScottyM ()
page = get "/:page" $ do
  pageName <- param "page"
  let filename = "static/build/" `T.append` pageName `T.append` ".html"
  file (T.unpack filename)
