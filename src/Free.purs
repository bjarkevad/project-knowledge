module Free where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Free (Free, foldFree, liftF)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.NaturalTransformation (NaturalTransformation)
import Data.Newtype (class Newtype)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Unsafe.Coerce (unsafeCoerce)

newtype TaskId = TaskId String
derive instance ntTI :: Newtype TaskId _
derive newtype instance rfTI :: ReadForeign TaskId
derive newtype instance wfTI :: WriteForeign TaskId

type Task = { id :: TaskId }

newtype ProjectId = ProjectId String
derive instance ntPI :: Newtype ProjectId _
derive newtype instance rfPI :: ReadForeign ProjectId
derive newtype instance wfPI :: WriteForeign ProjectId

type Project = { id :: ProjectId, tasks :: List Task }


data ProjectServiceF a = GetProjects (List Project -> a)
                       | Get ProjectId (Maybe Project -> a)
                       | Delete ProjectId (Boolean -> a)

type ProjectService = Free ProjectServiceF

get projectId = liftF $ Get projectId id

delete :: ProjectId -> Free ProjectServiceF Boolean
delete projectId = liftF $ Delete projectId id

undefined :: forall a. a
undefined = unsafeCoerce unit

interpret :: forall a eff. Free ProjectServiceF a -> Aff ( console :: CONSOLE | eff) a
interpret = foldFree go
            where
              go :: forall e. NaturalTransformation ProjectServiceF (Aff (console :: CONSOLE | e))
              go  = undefined
              -- go (Get thingId next) = do
              --   log $ "Getting " <> thingId
              --   pure $ next (Just {id: "yay"})
              -- go (Delete id next) = do
              --   log $ "Deleting " <> id
              --   pure $ next true
