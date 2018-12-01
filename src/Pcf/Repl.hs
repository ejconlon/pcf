module Pcf.Repl where

import           Data.Functor             (($>))
import           Data.Text                (Text)
import           Pcf.Cli                  (Cli, Command, ReplDirective(..), execCli, outputShow, outputStrLn, outputStr, repl)
import           Pcf.Functions            (bigStepTop, typeCheckTop)
import           Pcf.Parser               (readExp, sexp)
import           Pcf.Printer              (repExp)
import           Pcf.Types                (Exp (..), SExp (..))
import qualified Text.Megaparsec          as MP

data ReplState = ReplState deriving (Eq, Show)

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
        (outputStrLn "ERROR: Cannot typecheck exp")
        (\ty -> outputStr "Type: " >> outputShow ty)
        mty
    let mv = bigStepTop e
    maybe
        (outputStrLn "ERROR: Cannot evaluate exp")
        (\v -> outputStr "Value: " >> outputShow v)
        mv

    -- TODO strip names, typecheck, evaluate
handleSExp :: SExp Text -> Repl ()
handleSExp se = do
    outputStr "Parsed SExp: "
    outputShow se
    let me = readExp se
    case me of
        Nothing -> outputStrLn "ERROR: Cannot parse Exp"
        Just e  ->
            let rendered = repExp e
            in if (rendered /= se)
                then do
                    outputStrLn "ERROR: Mismatched pretty print"
                    outputShow rendered
                else handleExp e

replCommand :: ReplCommand
replCommand input = do
    case input of
        ":q" -> pure ReplQuit
        _ -> do
            let mse = MP.parseMaybe sexp input
            case mse of
                Nothing -> outputStrLn "ERROR: Cannot parse SExp"
                Just se -> handleSExp se
            pure ReplContinue

niceRepl :: Repl ()
niceRepl = do
    outputStrLn "Welcome to the PCF Repl."
    outputStrLn "Enter `:q` to exit."
    repl "> " replCommand

main :: IO ()
main = execCli niceRepl emptyReplState $> ()
