{-# LANGUAGE OverloadedStrings #-}

-- "THE BEER-WARE LICENSE" (Revision 42):
-- <lemonnierk@ulrar.net> wrote this file. As long as you retain this notice you
-- can do whatever you want with this stuff. If we meet some day, and you think
-- this stuff is worth it, you can buy me a beer in return

import Commands
import Notifications

import Network.SimpleIRC
import Data.Maybe
import Data.Ini
import Web.Hastodon                         (mkHastodonClientFromToken)
import Network.HTTP.Simple                  (HttpException)
import Control.Concurrent.Timer             (repeatedStart, newTimer)
import Control.Concurrent.Suspend.Lifted    (sDelay)
import Control.Exception                    (catch)
import System.Environment                   (getArgs)
import Data.Either.Utils                    (forceEither)
import qualified Data.List                  as L
import qualified Data.Text                  as T
import qualified Data.ByteString.Char8      as B

handleHttpExcept x =
  let _ = (x :: HttpException) in
  putStrLn "Error when trying to connect to mastodon"

-- Helper to parse config
getConfigVal config category name = forceEither $ lookupValue category name config

-- Callback used to start the timer calling the getNotifs function
-- after connecting to the IRC server
onNumeric client chan s m
  | mCode m == "001" = do
      timer <- newTimer
      repeatedStart timer (catch (getNotifs client chan s) handleHttpExcept) $ sDelay 60
      return ()
  | otherwise = return ()

main = do
  args <- getArgs
  if L.length args == 1
  then do
    val <- readIniFile $ L.head args
    let config = forceEither val
    let getConfigM = T.unpack . getConfigVal config "MASTODON"
    let getConfigI = T.unpack . getConfigVal config "IRC"
    let mtoken  = getConfigM "token"
    let mdomain = getConfigM "domain"
    let ichan   = getConfigI "chan"
    let iserv   = getConfigI "server"
    let inick   = getConfigI "nick"
    let iadmins = L.map (B.pack . T.unpack . T.strip) $ T.splitOn "," $ getConfigVal config "IRC" "admins"
    let client = mkHastodonClientFromToken mdomain mtoken
    let events = [(Privmsg (\x y -> catch (onMessage client iadmins x y) handleHttpExcept))
                 ,(Numeric (onNumeric client $ B.pack ichan))]
    let ircServer = (mkDefaultConfig iserv inick)
                   { cChannels = [ichan]
                   , cEvents   = events
                   }
    connect ircServer False True
    putStrLn "end"
  else
    putStrLn "usage: umrc /path/to/config.ini"

