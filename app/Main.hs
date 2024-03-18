{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.List (find)
import Network.Socket
import Network.Socket.ByteString
import System.IO (BufferMode (..), hSetBuffering, stdout)

import Control.Concurrent

import System.Directory (doesFileExist)
import System.Environment (getArgs)

parsePath :: ByteString -> ByteString
parsePath = (!! 1) . BC.words . head . BC.lines

responseOk :: ByteString
responseOk = "HTTP/1.1 200 Ok\r\n\r\n"

responseNotFound :: ByteString
responseNotFound = "HTTP/1.1 404 Not Found\r\n\r\n"

responseWithBody :: ByteString -> ByteString -> ByteString
responseWithBody contentType body =
    "HTTP/1.1 200 OK\r\nContent-Type: "
        <> contentType
        <> "\r\nContent-Length: "
        <> BC.pack (show $ BC.length body)
        <> "\r\n\r\n"
        <> body
        <> "\r\n\r\n"

routes :: ByteString -> IO ByteString
routes req
    | "/" == path = pure responseOk
    | "/user-agent" == path = pure $ userAgentRoute req
    | "/echo/" `BC.isPrefixOf` path = pure $ echoRoute path
    | "/files/" `BC.isPrefixOf` path = filesRoute path
    | otherwise = pure responseNotFound
  where
    path = parsePath req

handleClient :: Socket -> IO ()
handleClient clientSocket = do
    req <- recv clientSocket 4096
    res <- routes req
    sendAll clientSocket res
    close clientSocket

userAgentRoute :: ByteString -> ByteString
userAgentRoute req =
    let
        requestParams = map BC.init $ BC.lines req
        maybeUserAgent = find (BC.isPrefixOf "User-Agent:") requestParams
     in
        maybe responseNotFound (responseWithBody "text/plain" . BC.drop 12) maybeUserAgent

echoRoute :: ByteString -> ByteString
echoRoute path =
    let abc = BC.drop 6 path
     in responseWithBody "text/plain" abc

filesRoute :: ByteString -> IO ByteString
filesRoute path = do
    directory <- getDirectory
    let filename = BC.unpack $ BC.drop 7 path
    let filepath = directory ++ filename

    fileExists <- doesFileExist filepath
    if fileExists
        then do
            contents <- readFile filepath
            pure $ responseWithBody "application/octet-stream" $ BC.pack contents
        else do
            pure responseNotFound

getDirectory :: IO String
getDirectory = do
    args <- getArgs
    case args of
        [_, dir] -> return dir
        _ -> return "files/"

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

        forkIO $ handleClient clientSocket
