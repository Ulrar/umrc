{-# LANGUAGE OverloadedStrings #-}

-- "THE BEER-WARE LICENSE" (Revision 42):
-- <lemonnierk@ulrar.net> wrote this file. As long as you retain this notice you
-- can do whatever you want with this stuff. If we meet some day, and you think
-- this stuff is worth it, you can buy me a beer in return

module Twitter (tweet, replytweet, deletetweet, retweet) where

import Control.Lens
import Network.SimpleIRC                    (sendMsg)
import qualified Web.Twitter.Conduit        as Twitter
import qualified Web.Twitter.Types          as Twitter
import qualified Data.ByteString.Char8      as B
import qualified Data.Text                  as T

tweet mgr twinfo msg s chan = do
  let tmsg = B.drop 1 $ B.dropWhile (/= ' ') msg
  st <- Twitter.call twinfo mgr $ Twitter.update $ T.pack $ B.unpack tmsg
  sendMsg s chan $ B.pack $ "Tweeted ! (id : " ++ (show $ Twitter.statusId st) ++ ")"

replytweet mgr twinfo msg s chan = do
  let id = B.drop 1 $ B.dropWhile (/= ' ') msg
  case reads (B.unpack id) :: [(Integer,String)] of
    [(id', repl)] -> do
      st <- Twitter.call twinfo mgr $ Twitter.update (T.pack repl) & Twitter.inReplyToStatusId ?~ id'
      sendMsg s chan $ B.pack $ "Reply tweeted ! (id : " ++ (show $ Twitter.statusId st) ++ ")"
    _ -> sendMsg s chan "Usage : !replytweet <id> <text>"

deletetweet mgr twinfo msg s chan = do
  let id = (B.drop 1 $ B.dropWhile (/= ' ') msg)
  case reads (B.unpack id) :: [(Integer,String)] of
    [(id', "")] -> do
      st <- Twitter.call twinfo mgr $ Twitter.destroyId id'
      sendMsg s chan "Deleted !"
    _ -> sendMsg s chan "Usage : !delete <id>"

retweet mgr twinfo msg s chan = do
  let id = (B.drop 1 $ B.dropWhile (/= ' ') msg)
  case reads (B.unpack id) :: [(Integer,String)] of
    [(id', "")] -> do
      st <- Twitter.call twinfo mgr $ Twitter.retweetId id'
      sendMsg s chan $ B.pack $ "Retweeted ! (id : " ++ (show $ Twitter.rsId st) ++ ")"
    _ -> sendMsg s chan "Usage : !retweet <id>"
