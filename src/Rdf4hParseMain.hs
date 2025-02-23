```python
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Data.Char (isLetter)
import Data.List
import qualified Data.Map as Map
import Data.RDF
#if MIN_VERSION_base(4,9,0)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif
#endif
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf (hPrintf)

main :: IO ()
main = do
  (opts, args) <- getArgs >>= compilerOpts
  processHelp opts
  validateArgs args
  let settings = parseSettings opts args
  processOutputFormat (outputFormat settings)
  when (debug settings) $ displayDebugInfo settings

  let parseAndWrite = getParser settings >>= parseInput (inputUri settings) . parserType
  parseAndWrite >>= writeResult settings

processHelp :: [Flag] -> IO ()
processHelp opts = when (Help `elem` opts) $ putStrLn (usageInfo header options) >> exitSuccess

validateArgs :: [String] -> IO ()
validateArgs args = when (null args) $ ioError $ userError $ "\n\nINPUT-URI required\n\n" <> usageInfo header options

parseSettings :: [Flag] -> [String] -> Settings
parseSettings opts args = Settings {
  debug = Debug `elem` opts,
  inputUri = head args,
  inputFormat = getWithDefault (InputFormat "turtle") opts,
  outputFormat = getWithDefault (OutputFormat "ntriples") opts,
  inputBaseUri = getInputBaseUri (head args) args opts,
  outputBaseUri = getWithDefault (OutputBaseUri inputBaseUri) opts
}

processOutputFormat :: String -> IO ()
processOutputFormat outputFormat = unless (outputFormat == "ntriples" || outputFormat == "turtle") $ do
  hPrintf stderr ("'" <> outputFormat <> "' is not a valid output format. Supported output formats are: ntriples, turtle\n")
  exitWith (ExitFailure 1)

displayDebugInfo :: Settings -> IO ()
displayDebugInfo settings =
  let Settings{inputUri, inputFormat, inputBaseUri, outputFormat, outputBaseUri} = settings
  in do
    hPrintf stderr "      INPUT-URI:  %s\n\n" inputUri
    hPrintf stderr "   INPUT-FORMAT:  %s\n" inputFormat
    hPrintf stderr " INPUT-BASE-URI:  %s\n\n" inputBaseUri
    hPrintf stderr "  OUTPUT-FORMAT:  %s\n" outputFormat
    hPrintf stderr "OUTPUT-BASE-URI:  %s\n\n" outputBaseUri

getParser :: Settings -> Either String (String -> RDFParser)
getParser Settings{inputFormat} = case inputFormat of
  "turtle" -> Right $ TurtleParser Nothing
  "ntriples" -> Right $ const NTriplesParser
  "xml" -> Right $ XmlParser Nothing
  str -> Left $ "Invalid format: " <> str

parseInput :: String -> RDFParser -> IO (Either ParseFailure (RDF TList))
parseInput uri parser = (if uri /= "-" then parseFile parser uri else parseString parser <$> TIO.getContents) >>= return

writeResult :: Settings -> Either ParseFailure (RDF TList) -> IO ()
writeResult settings (Left (ParseFailure msg)) = putStrLn msg >> exitWith (ExitFailure 1)
writeResult settings (Right rdfG) = doWriteRdf settings rdfG

doWriteRdf :: Settings -> RDF TList -> IO ()
doWriteRdf Settings{outputFormat, outputBaseUri} rdfG = case outputFormat of
  "turtle" -> writeRdf (TurtleSerializer (Just (T.pack outputBaseUri)) Map.empty) rdfG
  "ntriples" -> writeRdf NTriplesSerializer rdfG
  _ -> error $ "Unknown output format: " <> outputFormat

getInputBaseUri :: String -> [String] -> [Flag] -> String
getInputBaseUri inputUri args flags =
  if null $ tail args
    then getWithDefault (InputBaseUri inputUri) flags
    else getWithDefault (InputBaseUri (head $ tail args)) flags

isUri :: T.Text -> Bool
isUri str = not (T.null post) && T.all isLetter pre
  where
    (pre, post) = T.break (== ':') str

getWithDefault :: Flag -> [Flag] -> String
getWithDefault def args =
  case find (== def) args of
    Nothing -> strValue def
    Just val -> strValue val

strValue :: Flag -> String
strValue (InputFormat s) = s
strValue (InputBaseUri s) = s
strValue (OutputFormat s) = s
strValue (OutputBaseUri s) = s
strValue flag = error $ "No string value for flag: " <> show flag

data Flag
  = Help
  | Debug
  | InputFormat String
  | InputBaseUri String
  | OutputFormat String
  | OutputBaseUri String
  deriving (Show)

instance Eq Flag where
  Help == Help = True
  Debug == Debug = True
  InputFormat _ == InputFormat _ = True
  InputBaseUri _ == InputBaseUri _ = True
  OutputFormat _ == OutputFormat _ = True
  OutputBaseUri _ == OutputBaseUri _ = True
  _ == _ = False

data Settings = Settings {
  debug :: Bool,
  inputUri :: String,
  inputFormat :: String,
  outputFormat :: String,
  inputBaseUri :: String,
  outputBaseUri :: String
}

header :: String
header =
  "\nrdf4h_parse: an RDF parser and serializer\n\n"
    <> "\nUsage: rdf4h_parse [OPTION...] INPUT-URI [INPUT-BASE-URI]\n\n"
    <> "  INPUT-URI       a filename, URI or '-' for standard input (stdin).\n"
    <> "  INPUT-BASE-URI  the input/parser base URI or '-' for none.\n"
    <> "    Default is INPUT-URI\n"
    <> "    Equivalent to -I INPUT-BASE-URI, --input-base-uri INPUT-BASE-URI\n\n"

options :: [OptDescr Flag]
options =
  [ Option "h" ["help"] (NoArg Help) "Display this help, then exit",
    Option "d" ["debug"] (NoArg Debug) "Print debug info (like INPUT-BASE-URI used, etc.)",
    Option "i" ["input"] (ReqArg InputFormat "FORMAT") $
      "Set input format/parser to one of:\n"
        <> "  turtle      Turtle (default)\n"
        <> "  ntriples    N-Triples\n"
        <> "  xml         RDF/XML",
    Option "I" ["input-base-uri"] (ReqArg InputBaseUri "URI") $
      "Set the input/parser base URI. '-' for none.\n"
        <> "  Default is INPUT-BASE-URI argument value.\n\n",
    Option "o" ["output"] (ReqArg OutputFormat "FORMAT") $
      "Set output format/serializer to one of:\n"
        <> "  ntriples    N-Triples (default)\n"
        <> "  turtle      Turtle",
    Option "O" ["output-base-uri"] (ReqArg OutputBaseUri "URI") $
      "Set the output format/serializer base URI. '-' for none.\n"
        <> "  Default is input/parser base URI."
  ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o, n, []) -> return (o, n)
    (_, _, errs) -> ioError (userError ("\n\n" <> concat errs <> usageInfo header options))
```