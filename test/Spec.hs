import           Test.Hspec
import           TypeCheckSpec
import           EvalSpec

main :: IO ()
main = hspec $ do
  typecheckSpec
  evalSpec
