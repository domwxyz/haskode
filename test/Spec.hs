-- | Test suite runner for Haskode.
--
-- The focused test modules still use the same simple no-framework style:
-- each test returns Either String (), and this runner preserves a clear
-- module-level execution order.
module Main (main) where

import qualified Haskode.Test.Commands as Commands
import qualified Haskode.Test.Config   as Config
import qualified Haskode.Test.Core     as Core
import qualified Haskode.Test.Display  as Display
import qualified Haskode.Test.OpenAI   as OpenAI
import qualified Haskode.Test.Patch    as Patch
import qualified Haskode.Test.Policy   as Policy
import qualified Haskode.Test.Provider as Provider
import qualified Haskode.Test.Session  as Session
import qualified Haskode.Test.Tools    as Tools
import Haskode.Test.Util (runTests)

main :: IO ()
main = runTests $
     Core.tests
  ++ Config.tests
  ++ Policy.tests
  ++ Patch.tests
  ++ OpenAI.tests
  ++ Tools.tests
  ++ Session.tests
  ++ Commands.tests
  ++ Display.tests
  ++ Provider.tests
