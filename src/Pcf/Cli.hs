module Pcf.Cli where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStr, outputStrLn, runInputT)

-- TODO look into repline

type Handler = Text -> InputT IO ()

getInputLineText :: Text -> InputT IO (Maybe Text)
getInputLineText prompt = fmap (fmap T.pack) (getInputLine (T.unpack prompt))

outputStrText :: Text -> InputT IO ()
outputStrText = outputStr . T.unpack

outputStrLnText :: Text -> InputT IO ()
outputStrLnText = outputStrLn . T.unpack

printText :: Show a => a -> InputT IO ()
printText = outputStrLn . show

loop :: Handler -> InputT IO ()
loop handler = do
    minput <- getInputLineText "> "
    case minput of
        Nothing -> pure ()
        Just ":q" -> pure ()
        Just input -> do
            when (not (T.null input)) (handler input)
            loop handler

defaultHandler :: Handler
defaultHandler input = do
    outputStrText "Got: "
    outputStrLnText input

runMain :: Text -> Handler -> IO ()
runMain greeting handler = runInputT defaultSettings $ do
    outputStrLnText greeting
    outputStrLnText "Enter `:q` to quit."
    loop handler
