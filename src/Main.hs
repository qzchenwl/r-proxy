{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.ReverseProxy
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Data.Conduit.Network
import qualified Network.HTTP.Client as HC

main :: IO ()
main = do
    manager <- HC.newManager HC.defaultManagerSettings
    run 3000 $ waiProxyTo
        (\req -> do
            print req
            return (WPRProxyDest (ProxyDest {pdHost = "httpbin.org", pdPort = 80})))
        defaultOnExc
        manager

