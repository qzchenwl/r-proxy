{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Main where

import qualified Prelude
import ClassyPrelude
import Network.HTTP.ReverseProxy
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Data.Conduit.Network
import qualified Network.HTTP.Client as HC

import Text.Regex.PCRE (match)
import Safe (atMay)

import Conf
import Replace
import Utils

main :: IO ()
main = do
    port <- fromMaybe 3000 . listToMaybe . catMaybes . map readInt <$> getArgs
    Just (Conf rules) <- decodeFile "conf.yaml" :: IO (Maybe Conf)
    manager <- HC.newManager HC.defaultManagerSettings
    run port $ waiProxyTo (getReqDest rules) defaultOnExc manager

getReqDest rules req = do
    let url = toUrl req
        rule = find (\r -> match (unRegex r) (unpack url)) rules
    putStrLn $ "reqest url: " <> url
    case rule of
      Nothing -> return (WPRProxyDest (ProxyDest {pdHost = host, pdPort = port}))
          where (host, port) = getHostPort req
      Just r -> do
          let dest = proxyDest req r
              url' = toUrl req' where (WPRModifiedRequest req' _) = dest
          putStrLn $ "forward to: " <> url' <> "\n" <> pack (show r) <> "\n"
          return dest

proxyDest req rule = WPRModifiedRequest req' dest
    where req' = applyRule req rule
          dest = getDest req' rule

getDest req rule = ProxyDest { pdHost = fromMaybe host host', pdPort = fromMaybe port port' }
    where (host, port) = getHostPort req
          host' = encodeUtf8 <$> (unIp rule <|> unHost rule)
          port' = unPort rule

applyRule req rule = applyAll (catMaybes [ applyUrl (unRegex rule) <$> unUrl rule
                                         , applyHost <$> unHost rule]) req

applyUrl regex url req =
    case HC.parseUrl (unpack (replace (toUrl req) regex url)) of
      Nothing -> req
      Just req0 -> req
        { requestHeaders = update ("Host", HC.host req0) (requestHeaders req)
        , rawPathInfo = HC.path req0
        , rawQueryString = HC.queryString req0
        , requestHeaderHost = Just $ HC.host req0
        }

applyHost host req = req
    { requestHeaders = update ("Host", encodeUtf8 host) (requestHeaders req)
    , requestHeaderHost = Just (encodeUtf8 host)
    }

getHostPort req = (host, port)
    where headerHost = splitSeq ":" $ fromMaybe "localhost" $ requestHeaderHost req
          host = fromMaybe "localhost" $ atMay headerHost 0
          port = fromMaybe 80 $ join $ map (readInt.decodeUtf8) $ atMay headerHost 1

toUrl req = decodeUtf8 $ scheme <> "://" <> host <> path <> query
    where scheme = if isSecure req then "https" else "http"
          host = fromMaybe "localhost" $ requestHeaderHost req
          path = rawPathInfo req
          query = rawQueryString req


