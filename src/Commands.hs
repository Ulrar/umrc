{-# LANGUAGE OverloadedStrings #-}

-- "THE BEER-WARE LICENSE" (Revision 42):
-- <lemonnierk@ulrar.net> wrote this file. As long as you retain this notice you
-- can do whatever you want with this stuff. If we meet some day, and you think
-- this stuff is worth it, you can buy me a beer in return

module Commands (onMessage) where

import Mastodon
import Twitter
import Web.Hastodon
import Web.Twitter.Conduit.Response
import Network.HTTP.Types.Status            (Status(..))
import Control.Exception                    (catch)
import Data.Maybe                           (fromJust)
import Network.SimpleIRC                    (sendMsg, mChan, mNick, mMsg)
import qualified Data.Text                  as T
import qualified Data.ByteString.Char8      as B
import qualified Data.List                  as L

cmdIfAdminM mastodon admins nick s chan client msg f =
  if mastodon
  then
    if L.elem nick admins
    then
      f client s msg chan
    else
      sendMsg s chan "Unauthorized"
  else
    sendMsg s chan "Mastodon is disabled"

cmdIfAdminT twitter admins nick s chan mgr twinfo msg f =
  if twitter
  then
    if L.elem nick admins
    then
      catch (f mgr twinfo msg s chan)
        (\x -> do
          case x of
            FromJSONError str -> sendMsg s chan $ B.pack $ "Error decoding response : " ++ str
            TwitterErrorResponse st _ (h:t) -> sendMsg s chan $ B.pack $ "Error " ++ (show $ statusCode st) ++ " : " ++ (T.unpack $ twitterErrorMessage h)
            TwitterUnknownErrorResponse st _ _ -> sendMsg s chan $ B.pack $ "Unknown error " ++ (show $ statusCode st)
            TwitterStatusError st _ _ -> sendMsg s chan $ B.pack $ "Status error " ++ (show $ statusCode st)
        )
    else
      sendMsg s chan "Unauthorized"
  else
    sendMsg s chan "Twitter is disabled"

-- Callback when someone talks on IRC
onMessage client mastodon twitter tmgr twinfo admins s m
  | B.isPrefixOf "|toot" msg = cmdIfAdminM mastodon admins nick s chan client msg toot
  | B.isPrefixOf "|reply" msg = cmdIfAdminM mastodon admins nick s chan client msg replytoot
  | B.isPrefixOf "|delete" msg = cmdIfAdminM mastodon admins nick s chan client msg deletetoot
  | B.isPrefixOf "|boost" msg = cmdIfAdminM mastodon admins nick s chan client msg (mid postReblog "boost")
  | B.isPrefixOf "|favorite" msg = cmdIfAdminM mastodon admins nick s chan client msg (mid postFavorite "favorite")
  | B.isPrefixOf "|unfavorite" msg = cmdIfAdminM mastodon admins nick s chan client msg (mid postUnfavorite "unfavorite")
  | B.isPrefixOf "|follow" msg = cmdIfAdminM mastodon admins nick s chan client msg (followUnfollow postFollow "follow")
  | B.isPrefixOf "|unfollow" msg = cmdIfAdminM mastodon admins nick s chan client msg (followUnfollow postUnfollow "unfollow")
  | B.isPrefixOf "!tweet" msg = cmdIfAdminT twitter admins nick s chan tmgr twinfo msg tweet
  | B.isPrefixOf "!reply" msg = cmdIfAdminT twitter admins nick s chan tmgr twinfo msg replytweet
  | B.isPrefixOf "!delete" msg = cmdIfAdminT twitter admins nick s chan tmgr twinfo msg deletetweet
  | B.isPrefixOf "!retweet" msg = cmdIfAdminT twitter admins nick s chan tmgr twinfo msg retweet
  | otherwise = return ()
  where chan = fromJust $ mChan m
        msg = mMsg m
        nick = fromJust $ mNick m


