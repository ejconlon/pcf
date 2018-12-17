module Pcf.V2.Repl where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State.Strict (get, put)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Pcf.Core.Cli               (Cli, Command, ReplDirective (..), outputPartsLn,
                                             outputPretty, outputStrLn)
import           Pcf.Core.NiceRepl          (OptionCommands, ReplDef (..), runRepl,
                                             throwCommandError)

type Repl = Cli ()
type ReplCommand = Command ()
type ReplOptionCommands = OptionCommands ()

execCommand :: ReplCommand
execCommand input = do
    outputStrLn "TODO: exec"
    pure ReplContinue

evalCommand :: ReplCommand
evalCommand input = do
    outputStrLn "TODO: eval"
    pure ReplContinue

additionalOptions :: ReplOptionCommands
additionalOptions = M.fromList
    [ ("eval", ("evaluate an expression", evalCommand))
    ]

exe :: IO ()
exe = runRepl (ReplDef "Welcome to the PCF V2 Repl." () additionalOptions execCommand)
