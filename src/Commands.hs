{-# LANGUAGE OverloadedStrings #-}

-- "THE BEER-WARE LICENSE" (Revision 42):
-- <lemonnierk@ulrar.net> wrote this file. As long as you retain this notice you
-- can do whatever you want with this stuff. If we meet some day, and you think
-- this stuff is worth it, you can buy me a beer in return

module Commands (onMessage) where

import Mastodon
import Web.Hastodon
import Data.Maybe                           (fromJust)
import Network.SimpleIRC                    (sendMsg, mChan, mNick, mMsg)
import qualified Data.ByteString.Char8      as B
import qualified Data.List                  as L

cmdIfAdmin mastodon admins nick s chan client msg f =
  if mastodon
  then
    if L.elem nick admins
    then
      f client s msg chan
    else
      sendMsg s chan "Unauthorized"
  else
    sendMsg s chan "Mastodon is disabled"

-- Callback when someone talks on IRC
onMessage client mastodon twitter admins s m
  | B.isPrefixOf "|toot" msg = cmdIfAdmin mastodon admins nick s chan client msg toot
  | B.isPrefixOf "|replytoot" msg = cmdIfAdmin mastodon admins nick s chan client msg reply
  | B.isPrefixOf "|delete" msg = cmdIfAdmin mastodon admins nick s chan client msg delete
  | B.isPrefixOf "|boost" msg = cmdIfAdmin mastodon admins nick s chan client msg (mid postReblog "boost")
  | B.isPrefixOf "|favorite" msg = cmdIfAdmin mastodon admins nick s chan client msg (mid postFavorite "favorite")
  | B.isPrefixOf "|unfavorite" msg = cmdIfAdmin mastodon admins nick s chan client msg (mid postUnfavorite "unfavorite")
  | B.isPrefixOf "|follow" msg = cmdIfAdmin mastodon admins nick s chan client msg (followUnfollow postFollow "follow")
  | B.isPrefixOf "|unfollow" msg = cmdIfAdmin mastodon admins nick s chan client msg (followUnfollow postUnfollow "unfollow")
  | otherwise = return ()
  where chan = fromJust $ mChan m
        msg = mMsg m
        nick = fromJust $ mNick m


