{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main where

import Network.Socket
import System.Console.Haskeline
import System.IO
import Control.Concurrent
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.List
import Data.Maybe

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

----------------
--    ECHO    --
----------------
wrapMessage :: Handle -> (Handle -> IO ()) -> IO ()
wrapMessage handle func = do
  hPutStrLn handle "Hello from Client! - Harrison"
  hGetLine handle >>= putStrLn

  func handle

  hPutStrLn handle "Bye from Client - Harrison"
  hGetLine handle >>= putStrLn

echo :: Handle -> IO ()
echo handle =  do
  forkIO $ fix $ \loop -> do
    line <- hGetLine handle
    putStrLn $ "Server: " ++ line
    loop
  -- TODO: can kill threads. Use this to cancel the communication
  getInput
    where getInput = do
            input <- getLine
            case input of
              "/bye" -> hPutStrLn handle input
              _ -> hPutStrLn handle input >> getInput

--------------------
--    TRANSFER    --
--------------------
transfer :: Handle -> IO ()
transfer handle = runInputT defaultSettings $ do
  filename <- getInputLine "f% "

  liftIO $ do
    contents <- C.readFile $ trim $ fromJust filename
    C.hPutStr handle contents
    hFlush handle
    hClose handle

----------------
--    CHAT    --
----------------
chat :: Handle -> IO ()
chat handle = do
  hSetBuffering stdout NoBuffering
  
  forkIO $ fix $ \loop -> do
    hGetLine handle >>= putStrLn
    loop

  sendMessage
    where sendMessage = do
            putStr "% "
            input <- getLine
            hPutStrLn handle input
            sendMessage

----------------
--    CALC    --
----------------
calc :: Handle -> IO ()
calc handle = runInputT (Settings noCompletion Nothing False) $ do
  mexpr <- getInputLine "c% "
  case mexpr of 
    Nothing -> return ()
    Just input -> liftIO $ hPutStrLn handle input

  liftIO $ do
    expr <- hGetLine handle
    case expr of
      "goodbye." -> putStrLn expr
      _ -> do
        putStrLn expr
        calc handle

------------------
--    CLIENT    --
------------------
chooseOperation :: Handle -> IO ()
chooseOperation handle = runInputT defaultSettings $ do
  moperation <- getInputLine "op% "
  case moperation of
    Nothing -> return ()
    Just "/echo" -> liftIO $ hPutStrLn handle (fromJust moperation) >> wrapMessage handle echo
    Just "/file" -> liftIO $ hPutStrLn handle (fromJust moperation) >> transfer handle
    Just "/chat" -> liftIO $ hPutStrLn handle (fromJust moperation) >> chat handle
    Just "/calc" -> liftIO $ hPutStrLn handle (fromJust moperation) >> calc handle
    Just _ -> runInputT defaultSettings $ do
      outputStrLn "Error: Invalid operation."
      liftIO $ chooseOperation handle

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  connect sock $ SockAddrInet 8740 $ tupleToHostAddress (127, 0, 0, 1)

  handle <- socketToHandle sock ReadWriteMode
  hSetBuffering handle NoBuffering

  chooseOperation handle

  close sock
