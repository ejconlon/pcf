{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pcf.Cli where

import           Control.Monad            (unless)
import           Control.Monad.Catch      (MonadThrow(..))
import           Control.Monad.IO.Class   (MonadIO(..))
import           Control.Monad.Reader     (MonadReader(..), ReaderT(..))
import           Control.Monad.State.Strict (MonadState(..))
import           Control.Monad.Trans      (lift)
import           Data.IORef               (IORef, newIORef, readIORef, writeIORef)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import qualified System.Console.Haskeline as H

data CliConfig = CliConfig
    { ccPrompt :: Text
    , ccGreeting :: Text
    , ccQuit :: Text
    } deriving (Eq, Show)

newtype Cli s a = Cli { unCli :: H.InputT (ReaderT (IORef s) IO) a }
    deriving (Functor, Applicative, Monad, MonadIO)

withStateRef :: (IORef s -> IO a) -> Cli s a
withStateRef f = Cli (lift ask >>= lift . lift . f)

instance MonadState s (Cli s) where
    get = withStateRef readIORef
    put v = withStateRef (flip writeIORef v)

instance MonadThrow (Cli s) where
    throwM = Cli . H.throwIO

getInputLine :: Text -> Cli s (Maybe Text)
getInputLine prompt = Cli (fmap (fmap T.pack) (H.getInputLine (T.unpack prompt)))

outputStr :: Text -> Cli s ()
outputStr = Cli . H.outputStr . T.unpack

outputParts :: [Text] -> Cli s ()
outputParts (x:xs) = outputStr x >> outputParts xs
outputParts [] = pure ()

outputStrLn :: Text -> Cli s ()
outputStrLn = Cli . H.outputStrLn . T.unpack

outputPartsLn :: [Text] -> Cli s ()
outputPartsLn xs = outputParts xs >> outputStrLn ""

outputShow :: Show a => a -> Cli s ()
outputShow = liftIO . print

type Command s = Text -> Cli s ()

loop :: CliConfig -> Command s -> Cli s ()
loop cc command = do
    minput <- getInputLine (ccPrompt cc)
    case minput of
        Nothing -> pure ()
        Just input -> do
            if input == ccQuit cc
                then pure ()
                else do
                    unless (T.null input) (command input)
                    loop cc command

repl :: CliConfig -> Command s -> Cli s ()
repl cc command = do
    outputStrLn (ccGreeting cc)
    outputPartsLn ["Enter `", ccQuit cc, "` to quit."]
    loop cc command

runRepl :: CliConfig -> Command s -> s -> IO s
runRepl cc command initState = do
    ref <- newIORef initState
    let actInput = unCli $ repl cc command
        actReader = H.runInputT H.defaultSettings actInput
    runReaderT actReader ref
    readIORef ref
