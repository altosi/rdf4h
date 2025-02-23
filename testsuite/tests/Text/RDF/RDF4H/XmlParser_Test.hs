module Text.RDF.RDF4H.XmlParser_Test
  (
    tests
  ) where

import Data.Semigroup ((<>))
import Test.Tasty
import Test.Tasty.HUnit as TU
import Test.Tasty.QuickCheck as QC

import qualified Data.Map as Map
import Data.RDF.Query
import Data.RDF.Graph.TList (TList)
import Data.RDF.Types
import qualified Data.Text.IO as TIO
import qualified Data.Text as T (Text, pack, unlines)
import Text.RDF.RDF4H.XmlParser
import Text.RDF.RDF4H.NTriplesParser
import Text.Printf

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests

tests :: [TestTree]
tests =
 [ testCase "simpleStriping1" test_simpleStriping1
 , testCase "simpleStriping2" test_simpleStriping2
 , testCase "simpleSingleton1" test_simpleSingleton1
 , testCase "simpleSingleton2" test_simpleSingleton2
 , testCase "vCardPersonal" test_parseXmlRDF_vCardPersonal
 , testCase "NML" test_parseXmlRDF_NML
 , testCase "NML2" test_parseXmlRDF_NML2
 , testCase "NML3" test_parseXmlRDF_NML3
 , testProperty "isomorphicRoundTrip" prop_isomorphicRoundTrip
 ]
 <>
 fmap (uncurry checkGoodOtherTest) otherTestFiles

otherTestFiles :: [(String, String)]
otherTestFiles = [ ("data/xml", "example07")
                 , ("data/xml", "example08")
                 , ("data/xml", "example10")
                 , ("data/xml", "example11")
                 , ("data/xml", "example12")
                 , ("data/xml", "example13")
                 , ("data/xml", "example14")
                 , ("data/xml", "example15")
                 , ("data/xml", "example16")
                 , ("data/xml", "example17")
                 , ("data/xml", "example18")
                 , ("data/xml", "example19")
                 , ("data/xml", "example20")
                 , ("data/xml", "example22")
                 ]

checkGoodOtherTest :: String -> String -> TestTree
checkGoodOtherTest dir fname =
    let expGr = loadExpectedGraph1 (printf "%s/%s.out" dir fname :: String)
        inGr  = loadInputGraph1 dir fname
    in doGoodConformanceTest expGr inGr $ printf "xml-%s" fname

loadExpectedGraph1 :: String -> IO (Either ParseFailure (RDF TList))
loadExpectedGraph1 fname = do
  content <- TIO.readFile fname
  return $ parseString NTriplesParser content

loadInputGraph1 :: String -> String -> IO (Either ParseFailure (RDF TList))
loadInputGraph1 dir fname =
  (parseString (XmlParser Nothing (mkDocUrl1 testBaseUri dir fname)) <$>
     TIO.readFile (printf "%s/%s.rdf" dir fname :: String))

doGoodConformanceTest   :: IO (Either ParseFailure (RDF TList)) ->
                           IO (Either ParseFailure (RDF TList)) ->
                           String -> TestTree
doGoodConformanceTest expGr inGr testname =
    let t1 = assertLoadSuccess (printf "expected (%s): " testname) expGr
        t2 = assertLoadSuccess (printf "   input (%s): " testname) inGr
        t3 = assertEquivalent testname expGr inGr
    in testGroup (printf "conformance-%s" testname) $ fmap (uncurry testCase) [("loading-expected-graph-data", t1), ("loading-input-graph-data", t2), ("comparing-graphs", t3)]

mkTextNode :: T.Text -> Node
mkTextNode = lnode . plainL

testParse :: T.Text -> RDF TList -> Assertion
testParse exRDF ex =
    case parsed of
      Right result ->
          assertBool
            ("expected: " <> show ex <> "but got: " <> show result)
            (isIsomorphic (result :: RDF TList) (ex :: RDF TList))
      Left (ParseFailure err) ->
          assertFailure err
  where parsed = parseString (XmlParser Nothing Nothing) exRDF

prop_isomorphicRoundTrip :: T.Text -> Bool
prop_isomorphicRoundTrip input =
  let parsed = parseString (XmlParser Nothing Nothing) input
  in case parsed of
       Right rdf -> isIsomorphic rdf rdf
       Left _ -> False

assertEquivalent :: Rdf a => String -> IO (Either ParseFailure (RDF a)) -> IO (Either ParseFailure (RDF a)) -> TU.Assertion
assertEquivalent testname r1 r2 = do
  gr1 <- r1
  gr2 <- r2
  case equivalent gr1 gr2 of
    Nothing    -> return ()
    (Just msg) -> fail $ "Graph " <> testname <> " not equivalent to expected:\n" <> msg

equivalent :: Rdf a => Either ParseFailure (RDF a) -> Either ParseFailure (RDF a) -> Maybe String
equivalent (Left _) _                = Nothing
equivalent _        (Left _)         = Nothing
equivalent (Right gr1) (Right gr2)   = test $! zip gr1ts gr2ts
  where
    gr1ts = uordered $ uniqTriplesOf gr1
    gr2ts = uordered $ uniqTriplesOf gr2
    test []           = Nothing
    test ((t1,t2):ts) =
      case compareTriple t1 t2 of
        Nothing -> test ts
        err     -> err
    compareTriple t1 t2 =
      if equalNodes s1 s2 && equalNodes p1 p2 && equalNodes o1 o2
        then Nothing
        else Just ("Expected:\n  " <> show t1 <> "\nFound:\n  " <> show t2 <> "\n")
      where
        (s1, p1, o1) = f t1
        (s2, p2, o2) = f t2
        f t = (subjectOf t, predicateOf t, objectOf t)
    equalNodes (BNode _) (BNodeGen _) = True
    equalNodes (BNodeGen _) (BNode _) = True
    equalNodes (BNodeGen _) (BNodeGen _) = True
    equalNodes (BNode _) (BNode _) = True
    equalNodes n1          n2           = n1 == n2

assertLoadSuccess :: String -> IO (Either ParseFailure (RDF TList)) -> TU.Assertion
assertLoadSuccess idStr exprGr = do
  g <- exprGr
  case g of
    Left (ParseFailure err) -> TU.assertFailure $ idStr  <> err
    Right _ -> return ()

handleLoad :: Either ParseFailure (RDF TList) -> Either ParseFailure (RDF TList)
handleLoad res =
  case res of
    l@(Left _)  -> l
    (Right gr)  -> Right $ mkRdf (fmap normalize (triplesOf gr)) (baseUrl gr) (prefixMappings gr)

normalize :: Triple -> Triple
normalize t = let s' = normalizeN $ subjectOf t
                  p' = normalizeN $ predicateOf t
                  o' = normalizeN $ objectOf t
              in  triple s' p' o'
normalizeN :: Node -> Node
normalizeN (BNodeGen i) = BNode (T.pack $ "_:genid" <> show i)
normalizeN n            = n

testBaseUri :: String
testBaseUri  = "http://www.w3.org/2001/sw/DataAccess/df1/tests/"

mkDocUrl1 :: String -> String -> String -> Maybe T.Text
mkDocUrl1 baseDocUrl dir fname = Just . T.pack $ printf "%s/%s/%s.rdf" baseDocUrl dir fname
