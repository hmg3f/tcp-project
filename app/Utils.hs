{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Utils where

import System.IO
import Control.Concurrent
import Control.Monad.Fix (fix)
import Control.Monad

----------------
--    ECHO    --
----------------
echo :: Handle -> String -> IO ()
echo handle side =  do
  reader <- forkIO $ fix $ \loop -> do
    line <- hGetLine handle
    case line of
      "/bye" -> void $ putStrLn ("Bye from " ++ side ++ " - Harrison")
      _ -> putStrLn (side ++ ": " ++ line) >> loop
    
  fix $ \loop -> do
    input <- getLine
    case input of
      "/bye" -> hPutStrLn handle input
      _ -> hPutStrLn handle input >> loop

  killThread reader
