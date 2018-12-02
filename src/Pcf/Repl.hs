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
import           Pcf.Ops
import           Pcf.Parser                 (readExp, sexpParser)
import           Pcf.Printer                (repExp)
import           Pcf.Types                  (Exp (..), SExp (..))
import qualified Text.Megaparsec            as MP

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
