{-# LANGUAGE ScopedTypeVariables #-}

module Pcf.Repl where

import           Control.Concurrent     (threadDelay)
import           Control.Exception      (Exception, SomeException)
import           Control.Monad          (forever, unless)
import           Control.Monad.Catch    (catch, throwM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State.Strict (get, put)
import           Data.Foldable          (for_)
import           Data.Functor           (($>))
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Typeable          (Typeable)
import           Pcf.Cli                (Cli, Command, ReplDirective (..), execCli,
                                         outputPartsLn, outputPretty, outputStrLn,
                                         repl)
import           Pcf.Functions          (emptyFauxState)
import           Pcf.Ops

type Repl = Cli OpsData
type ReplCommand = Command OpsData

data ReplExc =
      ExpectedNoInputError
    | MissingCommandError Text
    | OpsError
    deriving (Eq, Show, Typeable)
instance Exception ReplExc

liftOpsT :: OpsT IO a -> Repl (Either OpsExc a)
liftOpsT act = do
    st <- get
    (ea, st') <- liftIO (runOpsT act st)
    put st'
    pure ea

quickOpsT :: OpsT IO a -> Repl a
quickOpsT act = do
    ea <- liftOpsT act
    case ea of
        Left e -> do
            outputStrLn "OPS ERROR:"
            outputPretty e
            throwM OpsError
        Right a -> pure a

printCatch :: Command s -> Command s
printCatch command input = catch (command input) $ \(e :: ReplExc) -> do
    outputStrLn "REPL ERROR: "
    outputPretty e
    pure ReplContinue

type OptionCommands = Map Text (Text, ReplCommand)

assertEmpty :: Text -> Repl ()
assertEmpty input = unless (Text.null input) (throwM ExpectedNoInputError)

bareCommand :: Repl ReplDirective -> ReplCommand
bareCommand cmd input = assertEmpty input >> cmd

execCommand :: ReplCommand
execCommand input = do
    se <- quickOpsT (parseSExp input)
    outputStrLn "Parsed SExp: "
    outputPretty se
    st <- quickOpsT (parseStmt se)
    outputStrLn "Parsed Stmt: "
    outputPretty st
    quickOpsT (processStmt st)
    pure ReplContinue

evalCommand :: ReplCommand
evalCommand input = do
    se <- quickOpsT (parseSExp input)
    outputStrLn "Parsed SExp: "
    outputPretty se
    e <- quickOpsT (parseExp se)
    outputStrLn "Parsed Exp: "
    outputPretty e
    fvs <- quickOpsT (freeVarsOps e)
    outputStrLn "Free vars: "
    outputPretty fvs
    ec <- quickOpsT (closConvOps e)
    outputStrLn "Clos conv: "
    outputPretty ec
    el <- quickOpsT (lambdaLiftOps ec)
    outputStrLn "Lambda lift: "
    outputPretty el
    (ef, fs) <- quickOpsT (fauxOps emptyFauxState el)
    outputStrLn "Faux C: "
    outputPretty ef
    outputStrLn "Faux State: "
    outputPretty fs
    ty <- quickOpsT (typeCheckOps e)
    outputStrLn "Type: "
    outputPretty ty
    v <- quickOpsT (bigStepOps e)
    outputStrLn "Value: "
    outputPretty v
    pure ReplContinue

quitCommand :: ReplCommand
quitCommand = bareCommand (pure ReplQuit)

hangCommand :: ReplCommand
hangCommand = bareCommand $ do
    liftIO (forever (threadDelay maxBound))
    pure ReplContinue

clearCommand :: ReplCommand
clearCommand = bareCommand $ do
    quickOpsT clear
    pure ReplContinue

helpCommand :: ReplCommand
helpCommand = bareCommand $ do
    outputStrLn "Available commands:"
    for_ (Map.toList optionCommands) $ \(name, (desc, _)) -> outputPartsLn [":", name, "\t", desc]
    pure ReplContinue

dumpCommand :: ReplCommand
dumpCommand = bareCommand $ do
    outputStrLn "Repl state:"
    s <- get
    outputPretty s
    pure ReplContinue

optionCommands :: OptionCommands
optionCommands = Map.fromList
    [ ("quit", ("quit", quitCommand))
    , ("hang", ("hang the interpreter (for testing)", hangCommand))
    , ("clear", ("clear decl/defn history", clearCommand))
    , ("help", ("describe all commands", helpCommand))
    , ("eval", ("evaluate an expression", evalCommand))
    , ("exec", ("execute a statement", execCommand))
    , ("dump", ("dump debug state", dumpCommand))
    ]

outerCommand :: ReplCommand
outerCommand input =
    case Text.uncons input of
        Just (':', rest) -> do
            let (name, subInput) = Text.breakOn " " rest
            case Map.lookup name optionCommands of
                Nothing           -> throwM (MissingCommandError name)
                Just (_, command) -> command (Text.drop 1 subInput)
        _ -> execCommand input

niceRepl :: Repl ()
niceRepl = do
    outputStrLn "Welcome to the PCF Repl."
    outputStrLn "Enter `:quit` to exit or `:help` to see all commands."
    repl "> " (printCatch outerCommand)

main :: IO ()
main = execCli niceRepl emptyOpsData $> ()
