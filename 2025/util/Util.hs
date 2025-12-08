module Util where

import Debug.Trace (traceWith)

traceLabel :: Show a => String -> a -> a
traceLabel l = traceWith (\a -> l ++ ": " ++ show a)
