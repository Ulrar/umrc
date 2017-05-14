{-# LANGUAGE OverloadedStrings #-}

-- "THE BEER-WARE LICENSE" (Revision 42):
-- <lemonnierk@ulrar.net> wrote this file. As long as you retain this notice you
-- can do whatever you want with this stuff. If we meet some day, and you think
-- this stuff is worth it, you can buy me a beer in return

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

-- Helper to call a function taking a numeric ID
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

followUnfollow f cmd client s msg chan = do
  let tmsg = (B.drop 1 $ B.dropWhile (/= ' ') msg)
  res <- getSearchedAccounts (B.unpack tmsg) client
  case res of
    Left (JSONParseException _ resp _) -> handleError resp s chan
    Left (JSONConversionException _ resp _) -> handleError resp s chan
    Right [] -> sendMsg s chan $ B.pack "Error : No such account"
    Right (x:[]) -> do
      res' <- f (accountId x) client
      case res' of
        Left (JSONParseException _ resp _) -> handleError resp s chan
        Left (JSONConversionException _ resp _) -> handleError resp s chan
        Right _ -> sendMsg s chan $ B.pack $ cmd ++ "ed !"
    Right (x:t) -> sendMsg s chan $ B.pack "Error : Search returned multiple accounts"
 
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
  | B.isPrefixOf "|follow" msg = cmdIfAdmin admins nick s chan client msg (followUnfollow postFollow "follow")
  | B.isPrefixOf "|unfollow" msg = cmdIfAdmin admins nick s chan client msg (followUnfollow postUnfollow "unfollow")
  | otherwise = return ()
  where chan = fromJust $ mChan m
        msg = mMsg m
        nick = fromJust $ mNick m


