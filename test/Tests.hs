import qualified ConfigTests
import qualified Git.BranchParserTests
import qualified Github.IntegrationTests
import qualified Github.RepositoryTests
import qualified IntegrationTests
import qualified ParserTests
import qualified UtilTests

import           Test.Framework

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
     UtilTests.tests
  ++ ParserTests.tests
  ++ ConfigTests.tests
  ++ IntegrationTests.tests
  ++ Git.BranchParserTests.tests
  ++ Github.IntegrationTests.tests
  ++ Github.RepositoryTests.tests
