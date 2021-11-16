module Logger where

import           Colog
import qualified Data.Text as T
import           GHC.Stack (HasCallStack, callStack)

logger :: HasCallStack => Severity -> T.Text -> IO ()
logger sev msg = richMessageAction <& Msg sev callStack msg
