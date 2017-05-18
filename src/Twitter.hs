{-# LANGUAGE OverloadedStrings #-}

-- "THE BEER-WARE LICENSE" (Revision 42):
-- <lemonnierk@ulrar.net> wrote this file. As long as you retain this notice you
-- can do whatever you want with this stuff. If we meet some day, and you think
-- this stuff is worth it, you can buy me a beer in return

module Twitter (tweet, replytweet, deletetweet, tid, tusrname, tgetLastId, handleTwitterException) where

import Web.Twitter.Conduit.Response
import Control.Lens
import Network.SimpleIRC                    (sendMsg)
import Network.HTTP.Types.Status            (Status(..))
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

tid f fd cmd mgr twinfo msg s chan = do
  let id = (B.drop 1 $ B.dropWhile (/= ' ') msg)
  case reads (B.unpack id) :: [(Integer,String)] of
    [(id', "")] -> do
      st <- Twitter.call twinfo mgr $ f id'
      sendMsg s chan $ B.pack $ cmd ++ "ed ! (id : " ++ (show $ fd st) ++ ")"
    _ -> sendMsg s chan $ B.pack $ "Usage : !" ++ cmd ++ " <id>"

tusrname f cmd mgr twinfo msg s chan = do
  let txt = (B.drop 1 $ B.dropWhile (/= ' ') msg)
  user <- Twitter.call twinfo mgr $ f $ Twitter.ScreenNameParam $ B.unpack txt
  sendMsg s chan $ B.pack $ (T.unpack $ Twitter.userScreenName user) ++ " " ++ cmd ++ "ed !"

tgetLastId :: Twitter.Manager -> Twitter.TWInfo -> IO Integer
tgetLastId mgr twinfo = do
  st <- Twitter.call twinfo mgr $ Twitter.mentionsTimeline & Twitter.count ?~ 1
  case st of
    []    -> return 0
    (x:t) -> return $ Twitter.statusId x

handleTwitterException s chan x = do
  case x of
    FromJSONError str -> sendMsg s chan $ B.pack $ "Error decoding response : " ++ str
    TwitterErrorResponse st _ (h:t) -> sendMsg s chan $ B.pack $ "Error " ++ (show $ statusCode st) ++ " : " ++ (T.unpack $ twitterErrorMessage h)
    TwitterUnknownErrorResponse st _ _ -> sendMsg s chan $ B.pack $ "Unknown error " ++ (show $ statusCode st)
    TwitterStatusError st _ _ -> sendMsg s chan $ B.pack $ "Status error " ++ (show $ statusCode st)

