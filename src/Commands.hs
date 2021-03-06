{-# LANGUAGE OverloadedStrings #-}

-- "THE BEER-WARE LICENSE" (Revision 42):
-- <lemonnierk@ulrar.net> wrote this file. As long as you retain this notice you
-- can do whatever you want with this stuff. If we meet some day, and you think
-- this stuff is worth it, you can buy me a beer in return

module Commands (onMessage) where

import Mastodon
import Twitter
import Web.Hastodon
import Control.Exception                    (catch)
import Data.Maybe                           (fromJust)
import Network.SimpleIRC                    (sendMsg, mChan, mNick, mMsg)
import qualified Web.Twitter.Conduit        as Twitter
import qualified Web.Twitter.Types          as Twitter
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
      catch (f mgr twinfo msg s chan) (handleTwitterException s chan)
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
  | (B.isPrefixOf "!retweet" msg || B.isPrefixOf "!rt" msg) = cmdIfAdminT twitter admins nick s chan tmgr twinfo msg (tid Twitter.retweetId Twitter.rsId "retweet")
  | B.isPrefixOf "!favorite" msg = cmdIfAdminT twitter admins nick s chan tmgr twinfo msg (tid Twitter.favoritesCreate Twitter.statusId "favorite")
  | B.isPrefixOf "!unfavorite" msg = cmdIfAdminT twitter admins nick s chan tmgr twinfo msg (tid Twitter.favoritesDestroy Twitter.statusId "unfavorite")
  | B.isPrefixOf "!follow" msg = cmdIfAdminT twitter admins nick s chan tmgr twinfo msg (tusrname Twitter.friendshipsCreate "follow")
  | B.isPrefixOf "!unfollow" msg = cmdIfAdminT twitter admins nick s chan tmgr twinfo msg (tusrname Twitter.friendshipsDestroy "unfollow")
  | otherwise = return ()
  where chan = fromJust $ mChan m
        msg = mMsg m
        nick = fromJust $ mNick m


