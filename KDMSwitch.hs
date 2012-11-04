module Main where

import Control.Applicative ((<*))
import Control.Monad (when)
import Data.Char (digitToInt)
import Data.List (find)
import qualified Data.Map as Map
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import System.IO
import System.Process (readProcessWithExitCode)
import Text.ParserCombinators.Parsec

data Flag = User String
  deriving (Eq)

data Command = Command {
  cmdStr :: String,
  cmdOptions :: [OptDescr Flag],
  cmdAction :: [Flag] -> [String] -> IO (), 
  cmdUsage :: String
}

parseOptions cmd argv = case Map.lookup cmd commandMap of
  Nothing -> usageError Nothing $ "Unknown command: "++cmd
  Just c -> case getOpt Permute (commonOptions ++ cmdOptions c) argv of
    (o,n,[]  ) -> return (cmdAction c, o, n)
    (_,_,errs) -> usageError (Just c) (concatMap (filter ('\n' /=)) errs)

commonOptions =
  [
  ]

commands =
  [ Command "list" [] listAct listUsage
  , Command "switch" switchOpts switchAct switchUsage
  , Command "help" [] helpAct helpUsage
  ]

commandMap = Map.fromList $ map (\c -> (cmdStr c, c)) commands


helpAct _ [] = printHelp
helpAct _ (c:[]) = case Map.lookup c commandMap of
  Just cmd -> printCmdHelp cmd
  Nothing  -> printHelp
helpAct _ _ = printCmdHelp (commandMap Map.! "help")

helpUsage = "<CMD>\n\n  Prints help text for the given command"


switchOpts =
  [ Option "u" ["user"] (ReqArg User "USER") "user to switch to"
  ]

switchAct flags _ = do
  sessions <- parseKdmSessions
  case find ((==) user . sessionUser) sessions of
    Nothing -> runKdmCtl' ["reserve"]
    Just s -> runKdmCtl' ["activate","vt"++(show $ sessionVt s)]
    where
      user = last [u | User u <- User "" : flags]
  

switchUsage = ""


listAct _ _ = do
  sessions <- parseKdmSessions
  putStrLn $ show sessions

listUsage = "\n\n  Lists the current sessions"


data Session = Session {
  sessionDpy :: Int,
  sessionVt :: Int,
  sessionUser :: String,
  sessionString :: String,
  sessionActive :: Bool
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

runKdmCtl opts = do
  (r,o,e) <- readProcessWithExitCode "kdmctl" opts ""
  if (r /= ExitSuccess)
    then failure "Failed running kdmctl"
    else return (r,o,e)

runKdmCtl' opts = do
  runKdmCtl opts
  return ()

parseKdmSessions = do
  (r,o,e) <- runKdmCtl ["list", "alllocal"]
  case parse kdmCtlOutput "" o of
    Left e   -> failure "BUG -- Can't parse kdmctl output"
    Right ss -> return ss


main = do
  args <- getArgs
  when (null args) (usageError Nothing "No command given")
  (cmd, flags, fs) <- parseOptions (head args) (tail args)
  cmd flags fs


printHelp = do
  putStrLn "Usage:\n  kdmswitch <COMMAND> [OPTION...]\n"
  putStrLn (usageInfo "Options valid for all commands:" commonOptions)
  putStrLn "Available commands:"
  putStrLn $ unlines $ map ("  "++) (Map.keys commandMap)

printCmdHelp (Command cn opts _ usage) = do
  putStrLn ("Usage:\n  kdmswitch "++cn++" "++usage++"\n")
  putStrLn (usageInfo "Options valid for all commands:" commonOptions)
  putStrLn (usageInfo ("Options valid for command \""++cn++"\":") opts)


failure err = do
  putStrLn err
  exitFailure

usageError Nothing msg = do
  printHelp
  fail msg

usageError (Just cmd) msg = do
  printCmdHelp cmd
  fail msg
