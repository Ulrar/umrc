{-# LANGUAGE OverloadedStrings #-}
module Commands (onMessage) where

import Web.Hastodon
import Data.Maybe                           (fromJust)
import Network.HTTP.Types.Status            (statusCode, statusMessage)
import Network.SimpleIRC                    (sendMsg, mChan, mNick, mMsg)
import Network.HTTP.Simple                  (JSONException(JSONParseException), getResponseStatus)
import qualified Data.List                  as L
import qualified Data.ByteString.Char8      as B

handleError resp s chan = do
  let code = show $ statusCode $ getResponseStatus resp
  let errmsg = show $ statusMessage $ getResponseStatus resp
  sendMsg s chan $ B.pack $ "Error parsing response, status was : " ++ code ++ " " ++ errmsg

toot client s msg chan = do
  let tmsg = (B.drop 1 $ B.dropWhile (/= ' ') msg)
  res <- postStatus (B.unpack tmsg) client
  case res of
    Left (JSONParseException _ resp _) -> handleError resp s chan
    Right _  -> sendMsg s chan "Tooted !"

reply client s msg chan = do
  let id = B.drop 1 $ B.dropWhile (/= ' ') msg
  case reads (B.unpack id) :: [(Int,String)] of
    [(id', repl)] -> do
      res <- postReplyStatus repl id' client
      case res of
        Left (JSONParseException _ resp _) -> handleError resp s chan
        Right _  -> sendMsg s chan "Reply tooted !"
    _ -> sendMsg s chan "Usage : |replytoot <id> <text>"

fob f cmd client s msg chan = do
  let id = (B.drop 1 $ B.dropWhile (/= ' ') msg)
  case reads (B.unpack id) :: [(Int,String)] of
    [(id', "")] -> do
      res <- f id' client
      case res of
        Left (JSONParseException _ resp _) -> handleError resp s chan
        Right _  -> sendMsg s chan $ B.pack $ cmd ++ "ed !"
    _ -> sendMsg s chan $ B.pack $ "Usage : |" ++ cmd ++ " <id>"

follow client s msg chan = do
  let tmsg = (B.drop 1 $ B.dropWhile (/= ' ') msg)
  res <- postStatus (B.unpack tmsg) client
  case res of
    Left (JSONParseException _ resp _) -> handleError resp s chan
    Right acc -> sendMsg s chan $ B.pack "Following !"

cmdIfAdmin admins nick s chan client msg f =
  if L.elem nick admins
  then
    f client s msg chan
  else
    sendMsg s chan "Unauthorized"

-- Callback when someone talks on IRC
onMessage client admins s m
  | B.isPrefixOf "|toot" msg = cmdIfAdmin admins nick s chan client msg toot
  | B.isPrefixOf "|replytoot" msg = cmdIfAdmin admins nick s chan client msg reply
  | B.isPrefixOf "|boost" msg = cmdIfAdmin admins nick s chan client msg (fob postReblog "boost")
  | B.isPrefixOf "|favorite" msg = cmdIfAdmin admins nick s chan client msg (fob postFavorite "favorite")
  | B.isPrefixOf "|unfavorite" msg = cmdIfAdmin admins nick s chan client msg (fob postUnfavorite "unfavorite")
  | B.isPrefixOf "|follow" msg = cmdIfAdmin admins nick s chan client msg follow
  | otherwise = return ()
  where chan = fromJust $ mChan m
        msg = mMsg m
        nick = fromJust $ mNick m


