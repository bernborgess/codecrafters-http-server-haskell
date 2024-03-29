{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent
import Control.Monad (forever)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.List (find)
import Network.Socket
import Network.Socket.ByteString
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, stdout)

parseHeader :: ByteString -> [ByteString]
parseHeader = BC.words . head . BC.lines

parseBody :: ByteString -> ByteString
parseBody req = if null s then "" else BC.drop 4 (head s)
  where
    (_, s) = break (BC.isPrefixOf "\r\n\r\n") (BC.tails req)

responseOk :: ByteString
responseOk = "HTTP/1.1 200 Ok\r\n\r\n"

responseNotFound :: ByteString
responseNotFound = "HTTP/1.1 404 Not Found\r\n\r\n"

responseCreated :: ByteString
responseCreated = "HTTP/1.1 201 Created\r\n\r\n"

responseWithBody :: ByteString -> ByteString -> ByteString
responseWithBody contentType body = "HTTP/1.1 200 OK\r\nContent-Type: " <> contentType <> "\r\nContent-Length: " <> BC.pack (show $ BC.length body) <> "\r\n\r\n" <> body <> "\r\n\r\n"

routes :: ByteString -> IO ByteString
routes req
    | "/" == path = pure responseOk
    | "/user-agent" == path = pure $ userAgentRoute req
    | "/echo/" `BC.isPrefixOf` path = pure $ echoRoute path
    | "GET" == method && "/files/" `BC.isPrefixOf` path = getFilesRoute path
    | "POST" == method && "/files/" `BC.isPrefixOf` path = postFilesRoute path body
    | otherwise = pure responseNotFound
  where
    headers = parseHeader req
    method = head headers
    path = headers !! 1
    body = parseBody req

handleClient :: Socket -> IO ()
handleClient clientSocket = do
    req <- recv clientSocket 4096
    res <- routes req
    sendAll clientSocket res
    close clientSocket

userAgentRoute :: ByteString -> ByteString
userAgentRoute req = maybe responseNotFound (responseWithBody "text/plain" . BC.drop 12) maybeUserAgent
  where
    requestParams = map BC.init $ BC.lines req
    maybeUserAgent = find (BC.isPrefixOf "User-Agent:") requestParams

echoRoute :: ByteString -> ByteString
echoRoute path = responseWithBody "text/plain" abc
  where
    abc = BC.drop 6 path

getFilesRoute :: ByteString -> IO ByteString
getFilesRoute path = do
    filepath <- getFilePath path
    fileExists <- doesFileExist filepath
    if fileExists
        then do
            contents <- readFile filepath
            pure $ responseWithBody "application/octet-stream" $ BC.pack contents
        else pure responseNotFound

postFilesRoute :: ByteString -> ByteString -> IO ByteString
postFilesRoute path body = do
    filepath <- getFilePath path
    BC.writeFile filepath body
    return responseCreated

getFilePath :: ByteString -> IO String
getFilePath path = do
    directory <- getDirectory
    let filename = BC.unpack $ BC.drop 7 path
    return $ directory ++ filename

getDirectory :: IO String
getDirectory = do
    args <- getArgs
    case args of
        [_, dir] -> return dir
        _ -> return "files/"

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

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

    forever $ do
        (clientSocket, clientAddr) <- accept serverSocket
        BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."
        forkIO $ handleClient clientSocket
