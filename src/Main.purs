module Main where

import Prelude

import Control.Monad.Aff (Aff, Canceler, launchAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Free (Free, foldFree, liftF)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (NaturalTransformation)
import Data.Newtype (class Newtype)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query (liftAff)
import Halogen.VDom.Driver (runUI)
import Simple.JSON (class ReadForeign, class WriteForeign, writeJSON)
import Project as Project
import Unsafe.Coerce (unsafeCoerce)

main :: forall eff. Eff (HA.HalogenEffects _) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI mainComponent unit body

data MainQuery a = NoOp a
type MainState = {}

data ProjectSlot = ProjectSlot
derive instance eqProjectSlot :: Eq ProjectSlot
derive instance ordProjectSlot :: Ord ProjectSlot

initialMainModel :: MainState
initialMainModel = {}

mainComponent :: forall eff. H.Component HH.HTML MainQuery Unit Void (Aff _)
mainComponent = H.parentComponent { initialState: const initialMainModel
                                  , render
                                  , eval
                                  , receiver: const Nothing
                                  }
                 where
                   render :: MainState -> H.ParentHTML MainQuery _ ProjectSlot (Aff _)
                   render st = HH.div_
                               [ HH.h1_ [ HH.text "Project Knowledge"]
                                 , HH.div_
                                 [ HH.slot ProjectSlot Project.project unit (const Nothing) ]
                               ]
                   eval :: MainQuery ~> H.ParentDSL MainState MainQuery _ ProjectSlot Void (Aff _)
                   eval (NoOp next) = pure next
