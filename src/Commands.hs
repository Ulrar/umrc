{-# LANGUAGE OverloadedStrings #-}
module Commands (onMessage) where

import Data.Maybe                           (fromJust)
import Network.SimpleIRC                    (sendMsg, mChan, mNick, mMsg)
import Web.Hastodon                         (postStatus, postReplyStatus, postReblog)
import qualified Data.List                  as L
import qualified Data.ByteString.Char8      as B

-- Callback when someone talks on IRC
onMessage client admins s m
  | B.isPrefixOf "|toot" msg =
    if L.elem nick admins
    then do
      let tmsg = (B.drop 1 $ B.dropWhile (/= ' ') msg)
      res <- postStatus (B.unpack tmsg) client
      case res of
        Left err -> sendMsg s chan $ B.pack $ show err
        Right _  -> sendMsg s chan "Tooted !"
    else
      sendMsg s chan "Unauthorized"
  | B.isPrefixOf "|replytoot" msg =
    if L.elem nick admins
    then do
      let id = B.drop 1 $ B.dropWhile (/= ' ') msg
      case reads (B.unpack id) :: [(Int,String)] of
        [(id', repl)] -> do
          res <- postReplyStatus repl id' client
          case res of
            Left err -> sendMsg s chan $ B.pack $ show err
            Right _  -> sendMsg s chan "Reply tooted !"
        _ -> sendMsg s chan "Usage : !replytoot <id> <text>"
    else
      sendMsg s chan "Unauthorized"
  | B.isPrefixOf "|boost" msg =
    if L.elem nick admins
    then do
      let id = (B.drop 1 $ B.dropWhile (/= ' ') msg)
      case reads (B.unpack id) :: [(Int,String)] of
        [(id', "")] -> do
          res <- postReblog id' client
          case res of
            Left err -> sendMsg s chan $ B.pack $ show err
            Right _  -> sendMsg s chan "Boosted !"
        _ -> sendMsg s chan "Usage : !boost <id>"
    else
      sendMsg s chan "Unauthorized"
  | otherwise = return ()
  where chan = fromJust $ mChan m
        msg = mMsg m
        nick = fromJust $ mNick m


