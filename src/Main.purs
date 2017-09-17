module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff, Canceler, launchAff, runAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as EC
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Free (Free, foldFree, liftF)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (NaturalTransformation)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Halogen (liftEff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query (liftAff)
import Halogen.VDom.Driver (runUI)
import Project as Project
import Routing (matches, matchesAff)
import Routing.Match (Match)
import Routing.Match.Class (int, lit)
import Simple.JSON (class ReadForeign, class WriteForeign, writeJSON)
import Unsafe.Coerce (unsafeCoerce)

data Locations = Project Int | Home
instance showLocations :: Show Locations where
  show (Project i) = "Project " <> show i
  show (Home) = "Home"

derive instance eqLocations :: Eq Locations

home :: Match Locations
home = Home <$ lit ""

project :: Match Locations
project = Project <$> (lit "project" *> int)

routing :: Match Locations
routing = home <|> project

main :: forall eff. Eff (HA.HalogenEffects _) Unit
main = HA.runHalogenAff do
      body <- HA.awaitBody
      io <- runUI mainComponent unit body
      io.query <<< H.action <<< RouteChanged =<< matchesAff routing
      pure unit

data MainQuery a = RouteChanged (Tuple (Maybe Locations) Locations) a
type MainState = { location :: Locations }

data ProjectSlot = ProjectSlot
derive instance eqProjectSlot :: Eq ProjectSlot
derive instance ordProjectSlot :: Ord ProjectSlot

initialMainModel :: MainState
initialMainModel = { location: Home }

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
                                 case st.location of
                                   Project i -> [ HH.slot ProjectSlot Project.project unit (const Nothing) ]
                                   Home -> [HH.h2_ [HH.text "Home"]]
                               ]
                   eval :: MainQuery ~> H.ParentDSL MainState MainQuery _ ProjectSlot Void (Aff _)
                   eval (RouteChanged (Tuple old new) next) = do
                     whenM ((_ /= new) <<< _.location <$> H.get) $
                       H.modify (\st -> st {location = new})
                     pure next

