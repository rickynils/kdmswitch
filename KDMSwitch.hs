module Main where

import Control.Applicative ((<*))
import Data.Char (digitToInt)
import System.Exit
import System.Process (readProcessWithExitCode)
import Text.ParserCombinators.Parsec

data Session = Session {
  dpy :: Int,
  vt :: Int,
  user :: String,
  sessionString :: String,
  active :: Bool
} deriving Show

kdmCtlOutput :: GenParser Char st [Session]
kdmCtlOutput = string "ok" >> (many session) <* newline <* eof

session = do
  skipMany1 tab
  dpy <- char ':' >> digit
  char ','
  vt <- string "vt" >> digit
  char ','
  user <- many alphaNum
  char ','
  session <- many (noneOf [','])
  char ','
  active <- choice [char '*' >> return True, return False]
  return $ Session (digitToInt dpy) (digitToInt vt) user session active

failure err = do
  putStrLn err
  exitFailure

parseKdmSessions = do
  (ret,out,err) <- readProcessWithExitCode "kdmctl" ["list", "alllocal"] ""
  if ret /= ExitSuccess
    then failure "Failed running kdmctl"
    else 
      case parse kdmCtlOutput "" out of
        Left e   -> failure "Can't parse kdmctl output"
        Right ss -> return ss

main = do
  sessions <- parseKdmSessions
  putStrLn $ show sessions
