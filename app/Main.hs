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
      setHeader )
import qualified Data.ByteString.Lazy.Char8 as BS
import JsonInstance
import Run hiding (main)
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as T
import qualified Wrasse hiding (main)

main = scotty 5000 (
    -- ghc >> 
    typecheck >> home >> js >> css >> intro >> consent >> favicon)

----------------------------------------------
-- ghc :: ScottyM ()
-- ghc = post "/ghc" $ do
--     content <- body
--     let result = Wrasse.hook (BS.unpack content)
--     json $ return result
-- ----------------------------------------------

typecheck :: ScottyM ()
typecheck = post "/typecheck" $ do
    content <- body
    let result = processFile (BS.unpack content)
    json result


home :: ScottyM ()
home = get "/" $ do
    file "static/build/index.html"


intro :: ScottyM ()
intro = get "/intro" $ do
    file "static/build/introduction.html"

consent :: ScottyM ()
consent = get "/consent" $ do
    file "static/build/consent.html"

js :: ScottyM ()
js = get (regex  "^.*\\.js") $ do
    path <- param "0"
    let filename = "static/build" `T.append` path
    setHeader "Content-Type" "application/javascript"
    file (T.unpack filename)

css :: ScottyM ()
css = get (regex  "^.*\\.css") $ do
    path <- param "0"
    let filename = "static/build" `T.append` path
    setHeader "Content-Type" "text/css"
    file (T.unpack filename)

favicon :: ScottyM ()
favicon = get "/favicon.ico" $ do
    setHeader "Content-Type" "image/vnd.microsoft.icon"
    file "static/build/favicon.ico"