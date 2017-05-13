{-# LANGUAGE OverloadedStrings #-}
module Commands (onMessage) where

import Web.Hastodon
import Data.Maybe                           (fromJust)
import Network.HTTP.Types.Status            (statusCode, statusMessage)
import Network.SimpleIRC                    (sendMsg, mChan, mNick, mMsg)
import Network.HTTP.Simple                  (JSONException(JSONParseException, JSONConversionException), getResponseStatus)
import qualified Data.List                  as L
import qualified Data.ByteString.Char8      as B

handleError resp s chan = do
  let code = show $ statusCode $ getResponseStatus resp
  let errmsg = show $ statusMessage $ getResponseStatus resp
  sendMsg s chan $ B.pack $ "Error parsing response, status was : " ++ code ++ " " ++ errmsg

-- Reply to an existing toot, takes a numeric ID and text
reply client s msg chan = do
  let id = B.drop 1 $ B.dropWhile (/= ' ') msg
  case reads (B.unpack id) :: [(Int,String)] of
    [(id', repl)] -> do
      res <- postReplyStatus repl id' client
      case res of
        Left (JSONParseException _ resp _) -> handleError resp s chan
        Left (JSONConversionException _ resp _) -> handleError resp s chan
        Right _  -> sendMsg s chan "Reply tooted !"
    _ -> sendMsg s chan "Usage : |replytoot <id> <text>"

-- Helper to call function taking an numeric ID
mid f cmd client s msg chan = do
  let id = (B.drop 1 $ B.dropWhile (/= ' ') msg)
  case reads (B.unpack id) :: [(Int,String)] of
    [(id', "")] -> do
      res <- f id' client
      case res of
        Left (JSONParseException _ resp _) -> handleError resp s chan
        Left (JSONConversionException _ resp _) -> handleError resp s chan
        Right _  -> sendMsg s chan $ B.pack $ cmd ++ "ed !"
    _ -> sendMsg s chan $ B.pack $ "Usage : |" ++ cmd ++ " <id>"

-- Helper to call a function taking text
mtxt f cmd client s msg chan = do
  let tmsg = (B.drop 1 $ B.dropWhile (/= ' ') msg)
  res <- f (B.unpack tmsg) client
  case res of
    Left (JSONParseException _ resp _) -> handleError resp s chan
    Left (JSONConversionException _ resp _) -> handleError resp s chan
    Right _ -> sendMsg s chan $ B.pack $ cmd ++ "ed !"

cmdIfAdmin admins nick s chan client msg f =
  if L.elem nick admins
  then
    f client s msg chan
  else
    sendMsg s chan "Unauthorized"

-- Callback when someone talks on IRC
onMessage client admins s m
  | B.isPrefixOf "|toot" msg = cmdIfAdmin admins nick s chan client msg (mtxt postStatus "toot")
  | B.isPrefixOf "|replytoot" msg = cmdIfAdmin admins nick s chan client msg reply
  | B.isPrefixOf "|boost" msg = cmdIfAdmin admins nick s chan client msg (mid postReblog "boost")
  | B.isPrefixOf "|favorite" msg = cmdIfAdmin admins nick s chan client msg (mid postFavorite "favorite")
  | B.isPrefixOf "|unfavorite" msg = cmdIfAdmin admins nick s chan client msg (mid postUnfavorite "unfavorite")
  | B.isPrefixOf "|follow" msg = cmdIfAdmin admins nick s chan client msg (mtxt postFollow "follow")
  | B.isPrefixOf "|unfollow" msg = cmdIfAdmin admins nick s chan client msg (mtxt postUnfollow "unfollow")
  | otherwise = return ()
  where chan = fromJust $ mChan m
        msg = mMsg m
        nick = fromJust $ mNick m


