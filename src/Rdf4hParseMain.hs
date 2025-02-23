```haskell
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
#else
#endif
#else
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
  handleHelp opts
  validateArgs args opts
  let (config, mInputUri, docUri, emptyPms) = initializeConfig args opts
  processInputFormat config mInputUri docUri emptyPms

handleHelp :: [Flag] -> IO ()
handleHelp opts =
  when (Help `elem` opts) $ putStrLn (usageInfo header options) >> exitSuccess

validateArgs :: [String] -> [Flag] -> IO ()
validateArgs args opts =
  when (null args) $ ioError (userError ("\n\nINPUT-URI required\n\n" <> usageInfo header options))
  unless (formatValid (getFormatOpt OutputFormat "ntriples" opts)) $ invalidFormatExit

data Config = Config
  { debug :: Bool,
    inputUri :: String,
    inputFormat :: String,
    outputFormat :: String,
    inputBaseUri :: String,
    outputBaseUri :: String
  }

initializeConfig :: [String] -> [Flag] -> (Config, Maybe BaseUrl, Maybe T.Text, PrefixMappings)
initializeConfig args opts =
  let inputUri = head args
      debug = Debug `elem` opts
      inputFormat = getFormatOpt InputFormat "turtle" opts
      outputFormat = getFormatOpt OutputFormat "ntriples" opts
      inputBaseUri = getInputBaseUri inputUri args opts
      outputBaseUri = getFormatOpt OutputBaseUri inputBaseUri opts
      config = Config debug inputUri inputFormat outputFormat inputBaseUri outputBaseUri
      mInputUri = if inputBaseUri == "-" then Nothing else Just (BaseUrl (T.pack inputBaseUri))
      docUri = Just $ T.pack inputUri
      emptyPms = PrefixMappings Map.empty
  in (config, mInputUri, docUri, emptyPms)

processInputFormat :: Config -> Maybe BaseUrl -> Maybe T.Text -> PrefixMappings -> IO ()
processInputFormat config mInputUri docUri emptyPms =
  case (inputFormat config, isUri $ T.pack (inputUri config)) of
    ("turtle", True) ->
      parseAndWrite TurtleParser mInputUri docUri inputUri emptyPms (outputFormat config)
    ("turtle", False) ->
      handleLocalInput (TurtleParser mInputUri docUri) config inputUri emptyPms
    ("ntriples", True) ->
      parseAndWrite NTriplesParser Nothing docUri (inputUri config) emptyPms (outputFormat config)
    ("ntriples", False) ->
      handleLocalInput NTriplesParser config (inputUri config) emptyPms
    ("xml", True) ->
      parseAndWrite (XmlParser mInputUri docUri) mInputUri docUri (inputUri config) emptyPms (outputFormat config)
    ("xml", False) ->
      handleLocalInput (XmlParser mInputUri docUri) config (inputUri config) emptyPms
    (str, _) -> putStrLn ("Invalid format: " <> str) >> exitFailure

handleLocalInput :: (forall a. Rdf a => [T.Text] -> Parser a) -> Config -> String -> PrefixMappings -> IO ()
handleLocalInput parser config inputUri emptyPms = do
  contents <- if inputUri /= "-"
              then readFile inputUri
              else TIO.getContents
  parseAndWrite parser Nothing Nothing contents emptyPms (outputFormat config)

parseAndWrite :: (Rdf a) => Parser a -> Maybe BaseUrl -> Maybe T.Text -> String -> PrefixMappings -> String -> IO ()
parseAndWrite parser mInputUri docUri content emptyPms format =
  parseURL parser content >>= write format docUri emptyPms

write :: (Rdf a) => String -> Maybe T.Text -> PrefixMappings -> Either ParseFailure (RDF a) -> IO ()
write format docUri pms res = case res of
  (Left (ParseFailure msg)) -> putStrLn msg >> exitWith (ExitFailure 1)
  (Right rdfG) -> doWriteRdf rdfG
  where
    doWriteRdf rdfG = case format of
      "turtle" -> writeRdf (TurtleSerializer docUri pms) rdfG
      "ntriples" -> writeRdf NTriplesSerializer rdfG
      _ -> error $ "Unknown output format: " <> format

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

formatValid :: String -> Bool
formatValid fmt = fmt == "ntriples" || fmt == "turtle"

invalidFormatExit :: IO ()
invalidFormatExit = hPrintf stderr "Invalid output format. Supported output formats are: ntriples, turtle\n" >> exitWith (ExitFailure 1)

getFormatOpt :: (String -> Flag) -> String -> [Flag] -> String
getFormatOpt constructor defaultStr opts = case find (== constructor defaultStr) opts of
  Just (constructor str) -> str
  _ -> defaultStr

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