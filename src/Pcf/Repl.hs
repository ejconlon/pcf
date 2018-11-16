module Pcf.Repl where

import Data.Text (Text)
import Pcf.Cli (Handler, outputStrText, outputStrLnText, printText, runMain)
import Pcf.Functions (typeCheckTop, bigStepTop)
import Pcf.Parser (readExp, sexp)
import Pcf.Types (Exp(..), SExp(..))
import System.Console.Haskeline (InputT)
import qualified Text.Megaparsec as MP

handleExp :: Exp Text -> InputT IO ()
handleExp e = do
    outputStrText "Parsed Exp: "
    printText e
    let mty = typeCheckTop e
    maybe
        (outputStrLnText "ERROR: Cannot typecheck exp")
        (\ty -> outputStrText "Type: " >> printText ty)
        mty
    let mv = bigStepTop e
    maybe
        (outputStrLnText "ERROR: Cannot evaluate exp")
        (\v -> outputStrText "Value: " >> printText v)
        mv

    -- TODO strip names, typecheck, evaluate
handleSExp :: SExp Text -> InputT IO ()
handleSExp se = do
    outputStrText "Parsed SExp: "
    printText se
    let me = readExp se
    case me of
        Nothing -> outputStrLnText "ERROR: Cannot parse Exp"
        Just e -> handleExp e

replHandler :: Handler
replHandler input = do
    let mse = MP.parseMaybe sexp input
    case mse of
        Nothing -> outputStrLnText "ERROR: Cannot parse SExp"
        Just se -> handleSExp se

main :: IO ()
main = runMain "Welcome to the PCF Repl." replHandler
