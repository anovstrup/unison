module Unison.Util.Less where

--import           System.Exit                    (ExitCode)
import           System.Process
import           System.IO                      (Handle, hPutStr, hClose)
import Control.Concurrent (myThreadId, threadDelay)
--import           Control.Exception              (try)
import           Control.Exception.Extra        (ignore)
--import           Unison.Prelude                 (void, traceIO)
import           Unison.Prelude                 (traceIO, isJust)

createPager :: IO PagerHandle
createPager = do
  let args = ["--no-init"            -- don't clear the screen on exit
               ,"--raw-control-chars"  -- pass through colors and stuff
               ,"--prompt=[less] Use space/arrow keys to navigate, or 'q' to return to ucm:"
               ,"--quit-if-one-screen" -- self-explanatory
               ,"--quit-on-intr"
               ]
  (Just stdin, _stdout, _stderr, pid)
      <- createProcess (proc "less" args) { std_in = CreatePipe }

  return $ PH pid stdin

less :: String -> IO ()
less str = do
  traceIO "New less pager"
  p <- createPager
  -- If `less` exits before consuming all of stdin, `pPutStr` will crash.
  ignore $ pPutStr p str

  pWaitForTermination p

pPutStr :: PagerHandle -> String -> IO ()
pPutStr (PH pPid pStdin) str = do
  traceIO "Writing to pager"
  Just pid <- getPid pPid
  traceIO $ "(" ++ show pid ++ ")"
  ignore $ hPutStr pStdin str

pWaitForTermination :: PagerHandle -> IO ()
pWaitForTermination (PH pPid pStdin) = do
  tid <- myThreadId
  traceIO $ "Blocking thread " <> show tid
  -- If `less` has already exited, hClose throws an exception.
  ignore $ hClose pStdin

  -- wait for process to terminate
  spinWaitForProcess pPid

pTerminate :: PagerHandle -> IO ()
pTerminate (PH pPid pStdin) = do
  traceIO "Killing pager!!!"
  Just pid <- getPid pPid
  traceIO $ "(pid " ++ show pid ++ ")"
  -- If `less` has already exited, hClose throws an exception
  ignore $ hClose pStdin

  -- terminate process immediately
  terminateProcess pPid
--  signalProcess sigTERM pid

  traceIO $ "Waiting for pager to terminate... " <> show pid
  -- block until the process actually terminates
  spinWaitForProcess pPid
--  waitForProcess pPid
  --ignore $ void $ waitForProcess pPid
  traceIO $ "Killed pager " <> show pid

isRunning :: PagerHandle -> IO Bool
isRunning (PH pPid _) = do
  exitCode <- getProcessExitCode pPid
  case exitCode of
    Just _  -> pure False
    Nothing -> pure True

spinWaitForProcess :: ProcessHandle -> IO ()
spinWaitForProcess pid = do
  c <- getProcessExitCode pid
  if isJust c then return () else do
    threadDelay 100000
    spinWaitForProcess pid

data PagerHandle = PH ProcessHandle Handle
