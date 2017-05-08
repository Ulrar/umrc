{-# LANGUAGE OverloadedStrings #-}

-- "THE BEER-WARE LICENSE" (Revision 42):
-- <lemonnierk@ulrar.net> wrote this file. As long as you retain this notice you
-- can do whatever you want with this stuff. If we meet some day, and you think
-- this stuff is worth it, you can buy me a beer in return

import Network.SimpleIRC
import Data.Maybe
import Data.Ini
import Web.Hastodon
import Control.Concurrent.MVar
import Network.HTTP.Simple                  (HttpException)
import Control.Concurrent.Timer             (repeatedStart, newTimer)
import Control.Concurrent.Suspend.Lifted    (sDelay)
import Control.Monad                        (mapM_, when)
import Control.Exception                    (catch)
import System.Environment                   (getArgs)
import Text.HTML.TagSoup                    (parseTags, innerText)
import Data.Either.Utils                    (forceEither)
import qualified Data.List                  as L
import qualified Data.Text                  as T
import qualified Data.ByteString.Char8      as B

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
              sendMsg s chan $ B.pack (dn ++ " (" ++ nick ++ ")" ++ " started following")
              return ()
            "reblog" -> do
              when (isJust mstatus) $ do
                let t = parseTags $ statusContent $ fromJust mstatus
                let txt = innerText t
                let id = show $ statusId $ fromJust mstatus
                sendMsg s chan $ B.pack (dn ++ " (" ++ nick ++ ")" ++ " boosted : " ++ txt ++ "(id : " ++ id ++ ")")
              return ()
            "mention" -> do
              when (isJust mstatus) $ do
                let t = parseTags $ statusContent $ fromJust mstatus
                let txt = innerText t
                let id = show $ statusId $ fromJust mstatus
                sendMsg s chan $ B.pack (dn ++ " (" ++ nick ++ ")" ++ " : " ++ txt ++ " (id : " ++ id ++ ")")
              return ()
            _ -> return ()
        )
        notifs
      postNotificationsClear client
      return ()

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

-- Callback used to start the timer calling the getNotifs function
-- after connecting to the IRC server
onNumeric client chan s m
  | mCode m == "001" = do
      timer <- newTimer
      repeatedStart timer (catch (getNotifs client chan s) (\x -> let _ = (x :: HttpException) in putStrLn "Error when trying to connect to mastodon")) $ sDelay 60
      return ()
  | otherwise = return ()

main = do
  args <- getArgs
  if L.length args == 1
  then do
    val <- readIniFile $ L.head args
    let config = forceEither val
    let clientId = forceEither $ lookupValue "DEFAULT" "clientId" config
    let clientSecret = forceEither $ lookupValue "DEFAULT" "clientSecret" config
    let token = T.unpack $ forceEither $ lookupValue "DEFAULT" "token" config
    let domain = T.unpack $ forceEither $ lookupValue "DEFAULT" "domain" config
    let client = mkHastodonClientFromToken domain token
    let chan = T.unpack $ forceEither $ lookupValue "DEFAULT" "chan" config
    let chan' = B.pack chan
    let serv = T.unpack $ forceEither $ lookupValue "DEFAULT" "server" config
    let nick = T.unpack $ forceEither $ lookupValue "DEFAULT" "nick" config
    let admins = L.map (B.pack . T.unpack . T.strip) $ T.splitOn "," $ forceEither $ lookupValue "DEFAULT" "admins" config
    let events = [(Privmsg (onMessage client admins))
                 ,(Numeric (onNumeric client chan'))]
    let freenode = (mkDefaultConfig serv nick)
                   { cChannels = [chan]
                   , cEvents   = events
                   }
    connect freenode False True
    putStrLn "end"
  else
    putStrLn "usage: umrc /path/to/config.ini"

