module Main where

import Prelude

import Control.Monad.Aff (Canceler, launchAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Simple.JSON (writeJSON)
import Data.Maybe (Maybe(..))

main :: forall e. Eff ( exception :: EXCEPTION , console :: CONSOLE | e) (Canceler ( console :: CONSOLE | e))
main = launchAff $ do
       log $ writeJSON {token: "Thing2"}
