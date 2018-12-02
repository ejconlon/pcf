{-# LANGUAGE ScopedTypeVariables #-}

module Pcf.Repl where

import           Control.Concurrent         (threadDelay)
import           Control.Exception          (Exception, SomeException)
import           Control.Monad              (forever)
import           Control.Monad.Catch        (catch, throwM)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State.Strict (MonadState (..))
import           Data.Functor               (($>))
import           Data.Text                  (Text)
import           Data.Typeable              (Typeable)
import           Pcf.Cli                    (Cli, Command, ReplDirective (..), execCli,
                                             outputShow, outputStr, outputStrLn, repl)
import           Pcf.Functions              (bigStepTop, typeCheckTop)
import           Pcf.Parser                 (readExp, sexpParser)
import           Pcf.Printer                (repExp)
import           Pcf.Types                  (Exp (..), SExp (..))
import qualified Text.Megaparsec            as MP

data ReplState = ReplState deriving (Eq, Show)

data ReplExc =
      CannotTypeCheck (Exp Text)
    | CannotEvaluate (Exp Text)
    | CannotParseExp (SExp Text)
    | CannotParseSexp Text
    deriving (Eq, Show, Typeable)
instance Exception ReplExc

emptyReplState :: ReplState
emptyReplState = ReplState

type Repl = Cli ReplState
type ReplCommand = Command ReplState

handleExp :: Exp Text -> Repl ()
handleExp e = do
    outputStr "Parsed Exp: "
    outputShow e
    let mty = typeCheckTop e
    maybe
        (throwM (CannotTypeCheck e))
        (\ty -> outputStr "Type: " >> outputShow ty)
        mty
    let mv = bigStepTop e
    maybe
        (throwM (CannotEvaluate e))
        (\v -> outputStr "Value: " >> outputShow v)
        mv

    -- TODO strip names, typecheck, evaluate
handleSExp :: SExp Text -> Repl ()
handleSExp se = do
    outputStr "Parsed SExp: "
    outputShow se
    let me = readExp se
    case me of
        Nothing -> throwM (CannotParseExp se)
        Just e  ->
            let rendered = repExp e
            in if (rendered /= se)
                then do
                    outputStrLn "WARNING: Mismatched pretty print"
                    outputShow rendered
                else handleExp e

printCatch :: Command s -> Command s
printCatch command input = catch (command input) (\(e :: ReplExc) -> outputStr "ERROR: " >> outputShow e $> ReplContinue)

innerCommand :: ReplCommand
innerCommand input = do
    let mse = MP.parseMaybe sexpParser input
    case mse of
        Nothing -> throwM (CannotParseSexp input)
        Just se -> handleSExp se
    pure ReplContinue

outerCommand :: ReplCommand
outerCommand input = do
    case input of
        ":q" -> pure ReplQuit
        ":hang" -> do
            -- exists to exercise interrupt handling
            liftIO (forever (threadDelay maxBound))
            pure ReplContinue
        _ -> printCatch innerCommand input

niceRepl :: Repl ()
niceRepl = do
    outputStrLn "Welcome to the PCF Repl."
    outputStrLn "Enter `:q` to exit."
    repl "> " outerCommand

main :: IO ()
main = execCli niceRepl emptyReplState $> ()
