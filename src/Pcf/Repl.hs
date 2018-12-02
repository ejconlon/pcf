{-# LANGUAGE ScopedTypeVariables #-}

module Pcf.Repl where

import Control.Concurrent     (threadDelay)
import Control.Monad          (forever)
import Control.Monad.Catch    (catch)
import Control.Monad.IO.Class (liftIO)
import Data.Functor           (($>))
import Pcf.Cli                (Cli, Command, ReplDirective (..), execCli, outputShow,
                               outputStr, outputStrLn, repl)
import Pcf.Ops                (OpsData, OpsExc, bigStepOps, clear, emptyOpsData, interpretOps,
                               parseExp, parseSExp, typeCheckOps)

type Repl = Cli OpsData
type ReplCommand = Command OpsData

printCatch :: Command s -> Command s
printCatch command input = catch (command input) $ \(e :: OpsExc) -> do
    outputStr "ERROR: "
    outputShow e
    pure ReplContinue

innerCommand :: ReplCommand
innerCommand input = do
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

outerCommand :: ReplCommand
outerCommand input =
    case input of
        ":q"     -> pure ReplQuit
        ":hang"  -> liftIO (forever (threadDelay maxBound)) $> ReplContinue
        ":clear" -> interpretOps clear $> ReplContinue
        _        -> printCatch innerCommand input

niceRepl :: Repl ()
niceRepl = do
    outputStrLn "Welcome to the PCF Repl."
    outputStrLn "Enter `:q` to exit."
    repl "> " outerCommand

main :: IO ()
main = execCli niceRepl emptyOpsData $> ()
