{-# LANGUAGE OverloadedStrings #-}
module Commands (onMessage) where

import Data.Maybe                           (fromJust)
import Network.HTTP.Types.Status            (statusCode, statusMessage)
import Network.SimpleIRC                    (sendMsg, mChan, mNick, mMsg)
import Web.Hastodon                         (postStatus, postReplyStatus, postReblog)
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
    _ -> sendMsg s chan "Usage : !replytoot <id> <text>"

boost client s msg chan = do
  let id = (B.drop 1 $ B.dropWhile (/= ' ') msg)
  case reads (B.unpack id) :: [(Int,String)] of
    [(id', "")] -> do
      res <- postReblog id' client
      case res of
        Left (JSONParseException _ resp _) -> handleError resp s chan
        Right _  -> sendMsg s chan "Boosted !"
    _ -> sendMsg s chan "Usage : !boost <id>"

cmdIfAdmin admins nick s chan f =
  if L.elem nick admins
  then
    f chan
  else
    sendMsg s chan "Unauthorized"

-- Callback when someone talks on IRC
onMessage client admins s m
  | B.isPrefixOf "|toot" msg = cmdIfAdmin admins nick s chan (toot client s msg)
  | B.isPrefixOf "|replytoot" msg = cmdIfAdmin admins nick s chan (reply client s msg)
  | B.isPrefixOf "|boost" msg = cmdIfAdmin admins nick s chan (boost client s msg)
  | otherwise = return ()
  where chan = fromJust $ mChan m
        msg = mMsg m
        nick = fromJust $ mNick m


