import           Data.Char                                     (isAlphaNum)
import qualified Data.Map.Strict                               as Map
import qualified Data.Set                                      as Set
import           Data.Reflection

import           Text.Megaparsec
import           Text.Megaparsec.Char


import           Logic.Proof.Hilbert (emptyProof)
import           Logic.Matching.Rules.Kore ()
import           Logic.Matching.Rules.Minimal
import           Logic.Matching.Rules.Minimal.Syntax (parseMLRule)

import           Logic.Matching.Prover.Repl (ProverState(..), ProverEnv(..), runProver, execReplT, defaultSettings)
import           Logic.Matching.Prover.Command (Command, Parser, parseCommand)

import           Logic.Matching.Signature.Simple

import           Kore.AST.MetaOrObject (Meta)
import           Kore.AST.Common (SymbolOrAlias, Variable)
import           Kore.MetaML.AST (CommonMetaPattern)
import           Kore.Parser.Parser (metaPatternParser, metaHeadParser, metaVariableParser)
import           GlobalMain (defaultMainGlobal)

-- TODO: still needed?
parseName :: Parser String
parseName = takeWhile1P Nothing isAlphaNum <* space

pCommand :: Parser (Command String (MLRule (SymbolOrAlias Meta) (Variable Meta) CommonMetaPattern) CommonMetaPattern)
pCommand = parseCommand parseName parseFormula parseRule
  where
    parseFormula = metaPatternParser
    parseRule    = parseMLRule metaHeadParser
                               metaVariableParser
                               parseFormula
                               parseName

proveCommand
  :: (Reifies sig ValidatedSignature)
  => proxy (SimpleSignature sig)
  -> IO (ProverState String
          (MLRule (SymbolOrAlias Meta) (Variable Meta) CommonMetaPattern )
          (CommonMetaPattern))
proveCommand _ = do
  execReplT runProver defaultSettings proverEnv initialState
  where
    initialState = ProverState
                   { proofState = emptyProof
                   , commandStackState = []
                   }
    proverEnv    = ProverEnv
                   { commandParser = pCommand
                   , formulaVerifier = dummyFormulaVerifier
                   }

testSignature :: SignatureInfo
testSignature = SignatureInfo
  { sorts = Set.fromList ["Nat","Bool"]
  , labels = Map.fromList [("plus",("Nat",["Nat","Nat"]))
                          ,("succ",("Nat",["Nat"]))
                          ,("zero",("Nat",[]))
                          ]
  }

main :: IO ()
main =
  defaultMainGlobal
  >> ( case testSigValid of
         Nothing -> putStrLn $ show testSigValid
         Just validSig ->
           reifySignature
           validSig
           (\proxy -> proveCommand proxy
                      >> return ())
     )
  where testSigValid = validate testSignature
