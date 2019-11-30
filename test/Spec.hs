import EvalSpec
import Test.Hspec
import TypeCheckSpec

main :: IO ()
main = hspec $ do
  typecheckSpec
  evalSpec
