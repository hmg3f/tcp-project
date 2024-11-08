{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad (when)
import Control.Monad.Fix (fix)
import qualified Data.ByteString.Char8 as C
import Network.Socket
import System.IO
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.Printf

import qualified Utils as U

type Msg = (Int, String)

--------------------
--     PARSER     --
--------------------
data Expr = N Double
  | Neg Expr
  | Sum Expr Expr
  | Diff Expr Expr
  | Prod Expr Expr
  | Quot Expr Expr
  deriving (Show)

integer :: Parser String
integer = many1 digit

numberExpr  :: Parser Expr
numberExpr  = do
  out <- N . read <$> parser
  optional spaces
  return out
  where
    parser = (++) <$> integer <*> option "" ((:) <$> char '.'  <*> integer)

prefix :: String -> (a -> a) -> Operator Char () a
prefix name fun = Prefix $ do
  keyword name
  return fun

binary :: String -> (a -> a -> a) -> Operator Char () a
binary name fun = flip Infix AssocLeft $ do
  keyword name
  return fun

opTable :: OperatorTable Char () Expr
opTable = [[prefix "-" Neg],
           [binary "*" Prod, binary "/" Quot],
           [binary "+" Sum, binary "-" Diff]]

keyword :: String -> Parser ()
keyword kw = do
  try (string kw)
  spaces

parenExpr :: Parser Expr
parenExpr = do
  keyword "("
  ee <- expr
  keyword ")"
  return ee

operand :: Parser Expr
operand = try parenExpr <|> numberExpr

expr :: Parser Expr
expr = do
  ee <- exprParser
  spaces
  return ee
  where exprParser = buildExpressionParser opTable operand

parseExprStr :: Parser Expr
parseExprStr = do
  ee <- expr
  eof
  return ee

evalExprAST :: Expr -> Double
evalExprAST (N x) = x
evalExprAST (Neg x) = negate (evalExprAST x)
evalExprAST (Sum a b) = evalExprAST a + evalExprAST b
evalExprAST (Diff a b) = evalExprAST a - evalExprAST b
evalExprAST (Prod a b) = evalExprAST a * evalExprAST b
evalExprAST (Quot a b) = evalExprAST a / evalExprAST b

evalExprStr :: String -> String
evalExprStr str = getParsed $ parse parseExprStr [] str
  where getParsed (Left _) = "Error: Invalid expression entered."
        getParsed (Right r) = show $ evalExprAST r

----------------
--    ECHO    --
----------------
wrapMessage :: Handle -> (Handle -> IO ()) -> IO ()
wrapMessage handle func = do
  hGetLine handle >>= putStrLn
  hPutStrLn handle "Hello from Server! - Harrison"

  func handle

  hGetLine handle >>= putStrLn
  hPutStrLn handle "Bye from Server - Harrison"

echo :: Handle -> IO ()
echo handle =  do
  forkIO $ fix $ \loop -> do
    line <- hGetLine handle
    putStrLn $ "Client: " ++ line
    loop

  getServerInput
    where getServerInput = do
            input <- getLine
            case input of
              "/bye" -> hPutStrLn handle input
              _ -> hPutStrLn handle input >> getServerInput

--------------------
--    TRANSFER    --
--------------------
transfer :: Handle -> IO ()
transfer handle = do
  file <- C.hGetContents handle

  let file_mod = C.append file $ C.pack "\nAdded by server"

  C.putStrLn file_mod
  C.writeFile "/tmp/recv.txt" file_mod

----------------
--    CHAT    --
----------------
chat :: Handle -> Chan Msg -> Int -> IO ()
chat handle chan msgNum = do
  let broadcast msg = writeChan chan (msgNum, msg)
  
  hPutStrLn handle "Hi, what's your name?"
  name <- hGetLine handle
  broadcast ("--> " ++ name ++ " entered chat.")
  --hPutStrLn handle ("Welcome, " ++ name ++ "!")

  commLine <- dupChan chan

  -- fork off a thread for reading from the duplicated channel
  reader <- forkIO $ fix $ \loop -> do
    (nextNum, line) <- readChan commLine
    when (msgNum /= nextNum) $ hPutStrLn handle line
    loop

  E.handle (\(E.SomeException _) -> return ()) $ fix $ \loop -> do
    line <- hGetLine handle
    case line of
      -- If an exception is caught, send a message and break the loop
      "quit" -> hPutStrLn handle "Bye!"
      -- else, continue looping.
      _ -> broadcast (name ++ ": " ++ line) >> loop

  killThread reader                      -- kill after the loop ends
  broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast

----------------
--    CALC    --
----------------
calc :: Handle -> IO ()
calc handle = do
  client_expr <- hGetLine handle
  case client_expr of
    "/bye" -> hPutStrLn handle "goodbye."
    _ -> do
      hPutStrLn handle $ evalExprStr client_expr
      calc handle

------------------
--    SERVER    --
------------------
clientOperation :: Handle -> Chan Msg -> Int -> IO ()
clientOperation handle chan msgNum = do
  operation <- hGetLine handle

  case operation of
    "/echo" -> wrapMessage handle echo
    "/file" -> transfer handle
    "/chat" -> chat handle chan msgNum
    "/calc" -> calc handle
    x -> hPrintf handle "Invalid Command: %s" x

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan msgNum = do
    
  handle <- socketToHandle sock ReadWriteMode
  hSetBuffering handle NoBuffering
 
  clientOperation handle chan msgNum
    
  hClose handle                             -- close the handle

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
  conn <- accept sock
  forkIO (runConn conn chan msgNum)
  mainLoop sock chan $! msgNum + 1

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 8740 0)
  listen sock 2
  putStrLn "Listening on port 8740..."

  chan <- newChan

  _ <- forkIO $ fix $ \loop -> do
    (_, _) <- readChan chan
    loop

  mainLoop sock chan 0

  close sock

  -- (conn, _) <- accept sock
  -- putStrLn "New connection accepted"

  -- handle <- socketToHandle conn ReadWriteMode
  -- hSetBuffering handle NoBuffering

  -- clientOperation handle

  -- close conn
  -- close sock
