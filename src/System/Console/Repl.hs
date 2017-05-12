module System.Console.Repl where

import           Control.Monad
import           Control.Monad.Trans
import           Safe
import           System.Console.ANSI
import           System.Console.Haskeline

type Prompt = String
type InputLine = String
type Command a = (String, InputLine -> IO Bool)

defaultCommands :: [Command a]
defaultCommands =
  [(":q", const $ return False)]


repl :: Prompt -> (InputLine -> IO a) -> IO ()
repl prompt f =
  replWith prompt f defaultCommands


replWith :: Prompt -> (InputLine -> IO a) -> [Command a] -> IO ()
replWith prompt f commands =
  runInputT defaultSettings loop
  where loop =
          getInputLine prompt >>= maybe (return ()) pickCommand

        pickCommand input =
          case headMay (words input) >>= (`lookup` commands)  of
            Just cmd -> do
              continue <- liftIO $ cmd input
              when continue loop
            Nothing -> do
              liftIO $ f input
              loop


printError err = do
  setSGR [SetColor Foreground Vivid Red]
  print err
  setSGR [Reset]
