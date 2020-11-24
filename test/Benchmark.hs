module Benchmark where
import Control.Exception
import Formatting
import Formatting.Clock
import System.Clock


clock :: a -> IO ()
clock f = do
    start <- getTime Monotonic
    evaluate f
    end <- getTime Monotonic
    -- fprint (timeSpecs % "\n") start end
    fprint timeSpecs start end


