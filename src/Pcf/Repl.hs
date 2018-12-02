{-# LANGUAGE ScopedTypeVariables #-}

module Pcf.Repl where

import Control.Concurrent     (threadDelay)
import Control.Exception (Exception)
import Control.Monad          (forever, unless)
import Control.Monad.Catch    (catch, throwM)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Functor           (($>))
import Data.Map.Strict                  (Map)
import qualified Data.Map.Strict        as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import Pcf.Cli                (Cli, Command, ReplDirective (..), execCli, outputShow,
                               outputStr, outputStrLn, outputPartsLn, repl)
import Pcf.Ops                (OpsData, OpsExc, bigStepOps, clear, emptyOpsData, interpretOps,
                               parseExp, parseSExp, parseStmt, processStmt, typeCheckOps)

type Repl = Cli OpsData
type ReplCommand = Command OpsData

data ReplExc =
      ExpectedNoInput
    | MissingCommand Text
    deriving (Eq, Show, Typeable)
instance Exception ReplExc

printCatch :: Command s -> Command s
printCatch command input = catch (command input) $ \(e :: OpsExc) -> do
    outputStr "ERROR: "
    outputShow e
    pure ReplContinue

type OptionCommands = Map Text (Text, ReplCommand)

assertEmpty :: Text -> Repl ()
assertEmpty input = unless (Text.null input) (throwM ExpectedNoInput)

execCommand :: ReplCommand
execCommand input = do
    se <- interpretOps (parseSExp input)
    outputStr "Parsed SExp: "
    outputShow se
    st <- interpretOps (parseStmt se)
    outputStr "Parsed Stmt: "
    outputShow st
    interpretOps (processStmt st)
    pure ReplContinue

evalCommand :: ReplCommand
evalCommand input = do
    se <- interpretOps (parseSExp input)
    outputStr "Parsed SExp: "
    outputShow se
    e <- interpretOps (parseExp se)
    outputStr "Parsed Exp: "
    outputShow e
    ty <- interpretOps (typeCheckOps e)
    outputStr "Type: "
    outputShow ty
    v <- interpretOps (bigStepOps e)
    outputStr "Value: "
    outputShow v
    pure ReplContinue

quitCommand :: ReplCommand
quitCommand input = do
    assertEmpty input
    pure ReplQuit

hangCommand :: ReplCommand
hangCommand input = do
    assertEmpty input
    liftIO (forever (threadDelay maxBound))
    pure ReplContinue

clearCommand :: ReplCommand
clearCommand input = do
    assertEmpty input
    interpretOps clear
    pure ReplContinue

helpCommand :: ReplCommand
helpCommand input = do
    assertEmpty input
    outputStrLn "Available commands:"
    for_ (Map.toList optionCommands) $ \(name, (desc, _)) -> outputPartsLn [":", name, "\t", desc]
    pure ReplContinue

optionCommands :: OptionCommands
optionCommands = Map.fromList
    [ ("quit", ("quit", quitCommand))
    , ("hang", ("hang the interpreter (for testing)", hangCommand))
    , ("clear", ("clear decl/defn history", clearCommand))
    , ("help", ("describe all commands", helpCommand))
    , ("eval", ("evaluate an expression", evalCommand))
    , ("exec", ("execute a statement", execCommand))
    ]

outerCommand :: ReplCommand
outerCommand input = do
    case Text.uncons input of
        Just (':', rest) -> do
            let (name, subInput) = Text.breakOn " " rest
            case Map.lookup name optionCommands of
                Nothing -> throwM (MissingCommand name)
                Just (_, command) -> command (Text.drop 1 subInput)
        _ -> execCommand input

niceRepl :: Repl ()
niceRepl = do
    outputStrLn "Welcome to the PCF Repl."
    outputStrLn "Enter `:quit` to exit or `:help` to see all commands."
    repl "> " (printCatch outerCommand)

main :: IO ()
main = execCli niceRepl emptyOpsData $> ()
