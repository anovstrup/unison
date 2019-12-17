module Unison.Util.Less where

--import           System.Exit                    (ExitCode)
import           System.Process
import           System.IO                      (Handle, hPutStr, hClose)
--import           Control.Exception              (try)
import           Control.Exception.Extra        (ignore)
import           Unison.Prelude                 (void)


createPager :: IO PagerHandle
createPager = do
  let args = ["--no-init"            -- don't clear the screen on exit
               ,"--raw-control-chars"  -- pass through colors and stuff
               ,"--prompt=[less] Use space/arrow keys to navigate, or 'q' to return to ucm:"
               ,"--quit-if-one-screen" -- self-explanatory
               ]
  (Just stdin, _stdout, _stderr, pid)
      <- createProcess (proc "less" args) { std_in = CreatePipe, delegate_ctlc = True }

  return $ PH pid stdin

less :: String -> IO ()
less str = do
  putStrLn "New less pager"
  p <- createPager
  -- If `less` exits before consuming all of stdin, `pPutStr` will crash.
  ignore $ pPutStr p str

  pWaitForTermination p

pPutStr :: PagerHandle -> String -> IO ()
pPutStr (PH pPid pStdin) str = do
  putStrLn "Writing to pager"
  Just pid <- getPid pPid
  putStrLn $ "(" ++ show pid ++ ")"
  ignore $ hPutStr pStdin str

pWaitForTermination :: PagerHandle -> IO ()
pWaitForTermination (PH pPid pStdin) = do
  -- If `less` has already exited, hClose throws an exception.
  ignore $ hClose pStdin

  -- wait for process to terminate
  void $ waitForProcess pPid

pTerminate :: PagerHandle -> IO ()
pTerminate (PH pPid pStdin) = do
  putStrLn "Killing pager!!!"
  Just pid <- getPid pPid
  putStrLn $ "(pid " ++ show pid ++ ")"
  -- If `less` has already exited, hClose throws an exception
  ignore $ hClose pStdin

  -- terminate process immediately
  terminateProcess pPid

  -- block until the process actually terminates
  --ignore $ void $ waitForProcess pPid


isRunning :: PagerHandle -> IO Bool
isRunning (PH pPid _) = do
  exitCode <- getProcessExitCode pPid
  case exitCode of
    Just _  -> pure False
    Nothing -> pure True

data PagerHandle = PH ProcessHandle Handle

{-
cancellableWaitForProcess :: ProcessHandle -> IO ()
cancellableWaitForProcess pid = do
  code <- tryWait pid
  case code of
    Left _ -> void $ terminateProcess pid
    Right _ -> pure ()

tryWait :: ProcessHandle -> IO (Either Async.AsyncCancelled ExitCode)
tryWait pid = try $ waitForProcess pid-}
