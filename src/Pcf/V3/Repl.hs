module Pcf.V3.Repl where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State.Strict (get, put)
import           Data.Foldable              (traverse_)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Pcf.Core.Cli               (Cli, Command, ReplDirective (..), outputPartsLn,
                                             outputPretty, outputStrLn)
import           Pcf.Core.NiceRepl          (OptionCommands, ReplDef (..), runRepl,
                                             throwCommandError)
import           Pcf.V3.Ops
import           Pcf.V3.Prelude

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
    stx <- quickOpsT (parseStmt se)
    outputStrLn "Parsed StmtX: "
    outputPretty stx
    st0 <- quickOpsT (handleConvertStmt stx)
    outputStrLn "Converted Stmt0:"
    outputPretty st0
    quickOpsT (processStmt st0)
    pure ReplContinue

evalCommand :: ReplCommand
evalCommand input = do
    se <- quickOpsT (parseSExp input)
    outputStrLn "Parsed SExp: "
    outputPretty se
    ex <- quickOpsT (parseExp se)
    outputStrLn "Parsed ExpX: "
    outputPretty ex
    e0 <- quickOpsT (handleConvertExp ex)
    outputStrLn "Converted Exp0: "
    outputPretty e0
    fvs <- quickOpsT (freeVarsOps e0)
    outputStrLn "Free vars: "
    outputPretty fvs
    ty <- quickOpsT (typeCheckOps e0)
    outputStrLn "Type: "
    outputPretty ty
    v <- quickOpsT (traceBigStepOps e0)
    outputStrLn "Value: "
    outputPretty v
    (u, ks, eqs) <- quickOpsT (genDumpOps e0)
    outputStrLn "Gen dump: "
    outputPretty u
    outputPretty ks
    outputPretty eqs
    sd <- quickOpsT (solveDumpOps ks eqs)
    outputStrLn "Solve dump: "
    outputPretty sd
    ei <- quickOpsT (expandOps sd u)
    outputStrLn "Expanded: "
    outputPretty ei
    pure ReplContinue

additionalOptions :: ReplOptionCommands
additionalOptions = M.fromList
    [ ("eval", ("evaluate an expression", evalCommand))
    ]

exe :: IO ()
exe = do
    let (eu, d) = runOps (traverse_ processStmt prelude) emptyOpsData
    case eu of
        Left e -> do
            putStrLn "Error loading prelude:"
            print e
        Right () -> runRepl (ReplDef "Welcome to the PCF V3 Repl." d additionalOptions execCommand)
