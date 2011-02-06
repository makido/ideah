module HUtil where

import Outputable
import GHC
import PprTyThing

toString :: (Outputable a) => a -> String
toString x = show $ ppr x defaultUserStyle

toStringT :: Type -> String
toStringT t = show $ pprTypeForUser True t defaultUserStyle

unsupported :: String -> a -> a
unsupported str x = error str `seq` x
