{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pcf.Core.Cli where

import           Control.Monad                           (unless)
import           Control.Monad.Catch                     (MonadCatch (..), MonadThrow (..))
import           Control.Monad.IO.Class                  (MonadIO (..))
import           Control.Monad.Reader                    (MonadReader (..), ReaderT (..))
import           Control.Monad.State.Strict              (MonadState (..))
import           Control.Monad.Trans                     (lift)
import           Data.Functor                            (($>))
import           Data.IORef                              (IORef, newIORef, readIORef,
                                                          writeIORef)
import           Data.Text                               (Text)
import qualified Data.Text                               as T
import qualified Data.Text.IO                            as TIO
import qualified System.Console.Haskeline                as H
import qualified System.Console.Haskeline.MonadException as HE
import           Text.Pretty.Simple                      (pPrint)

newtype Cli s a = Cli { unCli :: H.InputT (ReaderT (IORef s) IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, HE.MonadException)

withStateRef :: (IORef s -> IO a) -> Cli s a
withStateRef f = Cli (lift ask >>= lift . lift . f)

instance MonadState s (Cli s) where
    get = withStateRef readIORef
    put v = withStateRef (`writeIORef` v)

instance MonadThrow (Cli s) where
    throwM = Cli . H.throwIO

instance MonadCatch (Cli s) where
    catch act f = Cli (H.catch (unCli act) (unCli . f))

getInputLine :: Text -> Cli s (Maybe Text)
getInputLine prompt = Cli (fmap (fmap T.pack) (H.getInputLine (T.unpack prompt)))

outputStr :: Text -> Cli s ()
outputStr = Cli . H.outputStr . T.unpack

outputParts :: [Text] -> Cli s ()
outputParts = foldr ((>>) . outputStr) (pure ())

outputStrLn :: Text -> Cli s ()
outputStrLn = Cli . H.outputStrLn . T.unpack

outputNewline :: Cli s ()
outputNewline = liftIO (putStrLn "")

outputPartsLn :: [Text] -> Cli s ()
outputPartsLn xs = outputParts xs >> outputStrLn ""

outputShow :: Show a => a -> Cli s ()
outputShow = liftIO . print

outputPretty :: Show a => a -> Cli s ()
outputPretty = pPrint

data ReplDirective = ReplQuit | ReplContinue deriving (Eq, Show)

type Command s = Text -> Cli s ReplDirective

repl :: Text -> Command s -> Cli s ()
repl prompt command = loop where
    loop = do
        minput <- H.handleInterrupt (pure (Just "")) (getInputLine prompt)
        case minput of
            Nothing -> pure ()
            Just input
                | T.null input -> loop
                | otherwise -> do
                    directive <- H.handleInterrupt (outputNewline $> ReplContinue) (command input)
                    case directive of
                        ReplQuit     -> pure ()
                        ReplContinue -> loop

runCli :: Cli s a -> s -> IO (a, s)
runCli cli initState = do
    ref <- newIORef initState
    let actReader = H.runInputT H.defaultSettings (H.withInterrupt (unCli cli))
    result <- runReaderT actReader ref
    finalState <- readIORef ref
    pure (result, finalState)

execCli :: Cli s () -> s -> IO s
execCli cli initState = snd <$> runCli cli initState
