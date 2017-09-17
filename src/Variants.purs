module Variants where

import Data.Variant
import Prelude

import Data.Maybe (Maybe)
import Type.Prelude (RProxy(..))


stringVariant :: forall r. Variant ( thing :: String | r)
stringVariant = inj (SProxy :: SProxy "thing") "the thing"

intVariant :: forall r. Variant ( intthing :: Int | r)
intVariant = inj (SProxy :: SProxy "intthing") 42

add1Maybe :: (Variant (intthing :: Int, thing :: String)) -> String
add1Maybe = case_
            # on (SProxy :: SProxy "intthing") (\i -> show $ i + 1)
            # on (SProxy :: SProxy "thing") id

maybeGetThing :: Variant (_) -> Maybe _
maybeGetThing = prj (SProxy :: SProxy "thing")

