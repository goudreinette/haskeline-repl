module System.Console.Repl where

import           Control.Monad.Trans
import           System.Console.Haskeline

type Prompt = String
type InputLine = String

repl :: Prompt -> (InputLine -> IO a) -> IO ()
repl prompt f =
  runInputT defaultSettings loop
  where
    loop = do
      line <- getInputLine prompt
      case line of
        Just input -> do
          liftIO $ f input
          loop
        Nothing ->
          return ()
