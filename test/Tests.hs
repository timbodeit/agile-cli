import qualified ParserTests
import qualified UtilTests
import qualified ConfigTests

import           Test.Framework

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
     UtilTests.tests
  ++ ParserTests.tests
  ++ ConfigTests.tests
