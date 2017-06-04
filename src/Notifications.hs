{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- "THE BEER-WARE LICENSE" (Revision 42):
-- <lemonnierk@ulrar.net> wrote this file. As long as you retain this notice you
-- can do whatever you want with this stuff. If we meet some day, and you think
-- this stuff is worth it, you can buy me a beer in return

module Notifications (getNotifs, getTMentions) where

import Web.Hastodon
import WordWrap
import Control.Lens
import Network.SimpleIRC                    (sendMsg)
import Control.Monad                        (mapM, mapM_, when)
import Data.Maybe                           (isJust, fromJust)
import Data.IORef                           (readIORef, writeIORef)
import Text.HTML.TagSoup                    (parseTags, innerText)
import qualified Web.Twitter.Conduit        as Twitter
import qualified Web.Twitter.Types          as Twitter
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.UTF8       as BU
import qualified Data.List                  as L
import qualified Data.Text                  as T

--
-- Mastodon
--

buildNotifPrefix dispName nick action = if L.length dispName > 0
  then
    dispName ++ " (" ++ nick ++ ") " ++ action
  else
    nick ++ " : " ++ action

dispStatus status action dn nick s chan = do
  let t = parseTags $ statusContent status
  let txt = innerText t
  let id = show $ statusId status
  let w = wrapLine 400 $ (buildNotifPrefix dn nick action) ++ txt ++ " (id : " ++ id ++ ")"
  case w of
    [] -> return ()
    (x:[]) -> sendMsg s chan $ BU.fromString x
    (x:t)  -> do
      sendMsg s chan $ BU.fromString $ x
      mapM_ (\y -> sendMsg s chan $ BU.fromString y) t

-- Connect to the API to get new notifs, print them on IRC then clear them
getNotifs client chan s = do
  eNotifs <- getNotifications client
  case eNotifs of
    Left _       -> return ()
    Right notifs -> do
      mapM_
        (\n -> do
         let acc = notificationAccount n
         let nick = accountAcct acc
         let dn = accountDisplayName acc
         let mstatus = notificationStatus n
         case notificationType n of
            "follow" -> do
              sendMsg s chan $ BU.fromString $ buildNotifPrefix dn nick "started following"
              return ()
            "reblog" -> do
              when (isJust mstatus) $ do
                dispStatus (fromJust mstatus) "boosted : " dn nick s chan
              return ()
            "mention" -> do
              when (isJust mstatus) $ do
                dispStatus (fromJust mstatus) "tooted : " dn nick s chan
              return ()
            _ -> return ()
        )
        notifs
      postNotificationsClear client
      return ()

--
-- Twitter
--

bSt status = (T.unpack $ Twitter.userScreenName $ Twitter.statusUser status) ++ " tweeted : " ++ (T.unpack $ Twitter.statusText status) ++ " (id : " ++ (show $ Twitter.statusId status) ++ ")"

dispStAndGetId s chan status = do
  sendMsg s chan $ BU.fromString $ bSt status
  return $ Twitter.statusId status

getTMentions lastid mgr twinfo chan s = do
  id <- readIORef lastid
  statuses <- Twitter.call twinfo mgr $ Twitter.mentionsTimeline & Twitter.sinceId ?~ id
  ids <- mapM (dispStAndGetId s chan) statuses
  writeIORef lastid $ maximum ids
  return ()
