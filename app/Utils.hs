{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Utils where

import System.IO
import Control.Concurrent
import Control.Monad.Fix (fix)

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
