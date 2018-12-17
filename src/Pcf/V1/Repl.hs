module Pcf.V1.Repl where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State.Strict (get, put)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Pcf.Core.Cli               (Cli, Command, ReplDirective (..), outputPartsLn,
                                             outputPretty, outputStrLn)
import           Pcf.Core.NiceRepl          (OptionCommands, ReplDef (..), runRepl,
                                             throwCommandError)
import           Pcf.V1.Functions           (emptyFauxState)
import           Pcf.V1.Ops

type Repl = Cli OpsData
type ReplCommand = Command OpsData
type ReplOptionCommands = OptionCommands OpsData

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
            throwCommandError
        Right a -> pure a

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

additionalOptions :: ReplOptionCommands
additionalOptions = M.fromList
    [ ("eval", ("evaluate an expression", evalCommand))
    ]

exe :: IO ()
exe = runRepl (ReplDef "Welcome to the PCF V1 Repl." emptyOpsData additionalOptions execCommand)
