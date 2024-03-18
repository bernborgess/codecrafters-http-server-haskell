{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Network.Socket
import Network.Socket.ByteString
import System.IO (BufferMode (..), hSetBuffering, stdout)

parsePath :: ByteString -> ByteString
parsePath = (!! 1) . BC.words . head . BC.lines

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    -- You can use print statements as follows for debugging, they'll be visible when running tests.
    BC.putStrLn "Logs from your program will appear here"

    -- Uncomment this block to pass first stage
    let host = "127.0.0.1"
        port = "4221"

    BC.putStrLn $ "Listening on " <> BC.pack host <> ":" <> BC.pack port

    -- Get address information for the given host and port
    addrInfo <- getAddrInfo Nothing (Just host) (Just port)

    serverSocket <- socket (addrFamily $ head addrInfo) Stream defaultProtocol

    setSocketOption serverSocket ReuseAddr 1
    withFdSocket serverSocket setCloseOnExecIfNeeded

    bind serverSocket $ addrAddress $ head addrInfo
    listen serverSocket 5

    -- Accept connections and handle them forever
    forever $ do
        (clientSocket, clientAddr) <- accept serverSocket
        BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."
        -- Handle the clientSocket as needed...

        req <- recv clientSocket 4096

        let path = parsePath req

        let res
                | path == "/" = "HTTP/1.1 200 OK\r\n\r\n"
                | path == "/user-agent" = userAgentRoute path
                | BC.isPrefixOf "/echo/" path = echoRoute path
                | otherwise = "HTTP/1.1 404 Not Found\r\n\r\n"

        sendAll clientSocket res
        close clientSocket

echoRoute :: ByteString -> ByteString
echoRoute path =
    let abc = BC.drop 6 path
     in "HTTP/1.1 200 OK\r\n\
        \Content-Type: text/plain\r\n\
        \Content-Length: "
            <> BC.pack (show $ BC.length abc)
            <> "\r\n\r\n"
            <> abc
            <> "\r\n\r\n"

userAgentRoute :: ByteString -> ByteString
userAgentRoute path = ""
