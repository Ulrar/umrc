{-# LANGUAGE OverloadedStrings #-}

-- "THE BEER-WARE LICENSE" (Revision 42):
-- <lemonnierk@ulrar.net> wrote this file. As long as you retain this notice you
-- can do whatever you want with this stuff. If we meet some day, and you think
-- this stuff is worth it, you can buy me a beer in return

module Notifications (getNotifs) where

import Web.Hastodon
import WordWrap
import Network.SimpleIRC                    (sendMsg)
import Control.Monad                        (mapM_, when)
import Data.Maybe                           (isJust, fromJust)
import Text.HTML.TagSoup                    (parseTags, innerText)
import qualified Data.ByteString.Char8      as B

buildNotifPrefix dispName nick action = dispName ++ " (" ++ nick ++ ") " ++ action

dispStatus status action dn nick s chan = do
  let t = parseTags $ statusContent status
  let txt = innerText t
  let id = show $ statusId status
  let w = wrapLine 400 $ (buildNotifPrefix dn nick action) ++ txt ++ " (id : " ++ id ++ ")"
  case w of
    [] -> return ()
    (x:[]) -> sendMsg s chan $ B.pack x
    (x:t)  -> do
      sendMsg s chan $ B.pack $ x
      mapM_ (\y -> sendMsg s chan $ B.pack y) t

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
              sendMsg s chan $ B.pack $ buildNotifPrefix dn nick "started following"
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

