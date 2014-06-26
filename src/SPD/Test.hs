module SPD.Test (
    module Test.Themis.Test
  , module Test.Themis.Test.Asserts
  , module Test.Themis.Provider.Interactive
  ) where

import Test.Themis.Test hiding (runTest)
import Test.Themis.Test.Asserts
import Test.Themis.Provider.Interactive hiding (runTest)