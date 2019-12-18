import DeclSpec
import EvalSpec
import ParserSpec
import Test.Hspec
import TypeCheckSpec

main :: IO ()
main = hspec $ do
  typeCheckSpec
  evalSpec
  parserSpec
  declSpec
