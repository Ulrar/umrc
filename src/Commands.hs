{-# LANGUAGE OverloadedStrings #-}

-- "THE BEER-WARE LICENSE" (Revision 42):
-- <lemonnierk@ulrar.net> wrote this file. As long as you retain this notice you
-- can do whatever you want with this stuff. If we meet some day, and you think
-- this stuff is worth it, you can buy me a beer in return

module Commands (onMessage) where

import Web.Hastodon
import Data.ByteString.Search               (replace)
import Data.ByteString.Lazy                 (toStrict)
import Data.Maybe                           (fromJust)
import Network.HTTP.Types.Status            (statusCode, statusMessage)
import Network.SimpleIRC                    (sendMsg, mChan, mNick, mMsg)
import Network.HTTP.Simple                  (JSONException(JSONParseException, JSONConversionException), getResponseStatus)
import qualified Data.List                  as L
import qualified Data.ByteString.Char8      as B
import qualified Data.Text                  as T

handleError resp s chan = do
  let code = show $ statusCode $ getResponseStatus resp
  let errmsg = show $ statusMessage $ getResponseStatus resp
  sendMsg s chan $ B.pack $ "Error parsing response, status was : " ++ code ++ " " ++ errmsg

-- Reply to an existing toot, takes a numeric ID and text
reply client s msg chan = do
  let id = B.drop 1 $ B.dropWhile (/= ' ') msg
  case reads (B.unpack id) :: [(Int,String)] of
    [(id', repl)] -> do
      let repl' = T.unpack $ T.replace "\\n" "\n" $ T.pack repl
      res <- if head repl' == '-'
        then
          let vis = T.unpack $ T.drop 1 $ T.takeWhile (/= ' ') $ T.pack repl' in
          postReplyStatusVis (T.unpack $ T.drop 1 $ T.dropWhile (/= ' ') $ T.pack repl') vis id' client
        else
          postReplyStatus repl' id' client
      case res of
        Left (JSONParseException _ resp _) -> handleError resp s chan
        Left (JSONConversionException _ resp _) -> handleError resp s chan
        Right st  -> sendMsg s chan $ B.pack $ "Reply tooted ! (id : " ++ (show $ statusId st) ++ ")"
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

toot client s msg chan = do
  let tmsg = toStrict $ replace (B.pack "\\n") (B.pack "\n") $ B.drop 1 $ B.dropWhile (/= ' ') msg
  res <- if B.head tmsg == '-'
    then
      let vis = B.unpack $ B.drop 1 $ B.takeWhile (/= ' ') tmsg in
      postStatusVis vis (B.unpack $ B.drop 1 $ B.dropWhile (/= ' ') tmsg) client
    else
      postStatus (B.unpack tmsg) client
  case res of
    Left (JSONParseException _ resp _) -> handleError resp s chan
    Left (JSONConversionException _ resp _) -> handleError resp s chan
    Right st -> sendMsg s chan $ B.pack $ "Tooted ! (id : " ++ (show $ statusId st) ++ ")"

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

delete client s msg chan = do
  let id = (B.drop 1 $ B.dropWhile (/= ' ') msg)
  case reads (B.unpack id) :: [(Int,String)] of
    [(id', "")] -> do
      res <- deleteStatus id' client
      if res
        then
          sendMsg s chan "Error when deleting toot"
        else
          sendMsg s chan "Deleted !"
    _ -> sendMsg s chan "Usage : |delete <id>"

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
  | B.isPrefixOf "|delete" msg = cmdIfAdmin admins nick s chan client msg delete
  | B.isPrefixOf "|boost" msg = cmdIfAdmin admins nick s chan client msg (mid postReblog "boost")
  | B.isPrefixOf "|favorite" msg = cmdIfAdmin admins nick s chan client msg (mid postFavorite "favorite")
  | B.isPrefixOf "|unfavorite" msg = cmdIfAdmin admins nick s chan client msg (mid postUnfavorite "unfavorite")
  | B.isPrefixOf "|follow" msg = cmdIfAdmin admins nick s chan client msg (followUnfollow postFollow "follow")
  | B.isPrefixOf "|unfollow" msg = cmdIfAdmin admins nick s chan client msg (followUnfollow postUnfollow "unfollow")
  | otherwise = return ()
  where chan = fromJust $ mChan m
        msg = mMsg m
        nick = fromJust $ mNick m


