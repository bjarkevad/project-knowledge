module Project where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (log)
import DOM.Event.Event (Event, preventDefault)
import DOM.HTML.Event.DataTransfer (getData, setData)
import DOM.HTML.Event.DragEvent (dataTransfer)
import DOM.HTML.Event.Types (DragEvent, dragEventToEvent)
import Data.Array (catMaybes, filter, head, snoc)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.MediaType.Common (textPlain)
import Data.Monoid (mempty)
import Data.Traversable (traverse)
import Data.UUID (UUID, genUUID, parseUUID)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query (action, liftEff)
import Text.Markdown.SlamDown.Halogen.Component (SlamDownQuery(SetDocument), slamDownComponent)
import Text.Markdown.SlamDown.Parser (parseMd)

type Project = { name :: String
               , nextId :: Int
               , taskLists :: Array TaskListId }

data ProjectQuery a = NewProject a
                    | UpdateName String a
                    | HandleTaskListMessage TaskListId TaskListMessage a

newtype TaskListSlot = TaskListSlot TaskListId
derive instance eqTaskListSlot :: Eq TaskListSlot
derive instance ordTaskListSlot :: Ord TaskListSlot

type TaskListId = Int
data TaskListQuery a = NewTask a
                     | HandleTaskMessage TaskId TaskMessage a
                     | OnDrop DragEvent a
                     | PreventDefault Event (TaskListQuery a)
                     | PopTask TaskId (Maybe Task -> a)
                     | AddTask Task (Unit -> a)
                     | NoOp a

type TaskList = { laneName :: String
                , tasks :: Array TaskId
                }

data TaskListMessage = NotifyTaskDropped TaskId

type TaskId = UUID
newtype TaskSlot = TaskSlot TaskId
derive instance eqTaskSlot :: Eq TaskSlot
derive instance ordTaskSlot :: Ord TaskSlot


data TaskQuery a = UpdateHeader String a
                 | UpdateContent String a
                 | Remove a
                 | IsDragging DragEvent a
                 | GetTask (Task -> a)
                 | SetTask Task (Unit -> a)

data TaskMessage = NotifyRemove | NotifyIsDragging

type Task = { taskId :: TaskId
            , header :: Maybe String
            , content :: Maybe String }

data SlamDownSlot = SlamDownSlot
derive instance eqSlamDownSlot :: Eq SlamDownSlot
derive instance ordSlamDownSlot :: Ord SlamDownSlot

initialProject :: Project
initialProject = { name: ""
                 , nextId: 0
                 , taskLists: [-1]
                 }

project :: H.Component HH.HTML ProjectQuery Unit Void (Aff _)
project = H.parentComponent { initialState: const initialProject
                             , render
                             , eval
                             , receiver: const Nothing }
           where
             render :: Project -> H.ParentHTML ProjectQuery TaskListQuery TaskListSlot (Aff _)
             render st = HH.div_
                         [ HH.input
                           [ HP.placeholder "Project name"
                           , HP.value st.name
                           , HE.onValueChange (HE.input UpdateName)]
                         , HH.br_
                         , HH.button [HE.onClick (HE.input_ NewProject)] [HH.text "New Lane"]
                         , HH.div_ (map renderTaskList st.taskLists)
                         ]

             renderTaskList :: TaskListId -> H.ParentHTML _ TaskListQuery _ (Aff _)
             renderTaskList taskListId  = HH.slot (TaskListSlot taskListId) taskList unit (HE.input (HandleTaskListMessage taskListId))

             eval :: ProjectQuery ~> H.ParentDSL _ _ TaskListQuery _ _ (Aff _)
             eval (NewProject next) = do
               H.modify (\st -> st {nextId = st.nextId + 1, taskLists = st.taskLists `snoc` st.nextId})
               liftAff $ log "NoOp"
               pure next
             eval (UpdateName n next) = do
               liftAff $ log "Updating name"
               H.modify (\st -> st {name = n})
               pure next
             eval (HandleTaskListMessage taskListId taskListMessage next) =
               case taskListMessage of
                 (NotifyTaskDropped taskId) -> do
                   liftAff <<< log $ "Task " <> show taskId <> " dropped on task list: " <> show taskListId
                   st <- H.get
                   taskMaybe <- head <<< catMaybes <$> traverse (queryPopTask taskId) (TaskListSlot <$> st.taskLists)
                   case taskMaybe of
                     Just task -> do
                       liftAff $ log "adding task"
                       void $ H.query (TaskListSlot taskListId) $ H.request (AddTask task)
                     Nothing -> do
                       liftAff $ log "couldn't find task"
                       pure unit
                   pure next

             queryPopTask :: UUID -> TaskListSlot -> H.HalogenM Project ProjectQuery TaskListQuery TaskListSlot Void (Aff _) (Maybe Task)
             queryPopTask taskId taskListSlot =
               join <$> (H.query taskListSlot $ H.request (PopTask taskId))

initialTaskList :: TaskList
initialTaskList = { laneName: "Backlog", tasks: mempty}

taskList :: H.Component HH.HTML TaskListQuery Unit TaskListMessage (Aff _)
taskList = H.parentComponent
            { initialState: const initialTaskList
            , render
            , eval
            , receiver: const Nothing
            }
            where
              render :: TaskList -> H.ParentHTML TaskListQuery TaskQuery TaskSlot (Aff _)
              render st =
                HH.div
                [ HE.onDrop $ (\e -> pure $ PreventDefault (dragEventToEvent e) (action $ OnDrop e))
                , HE.onDragOver $ (\e -> pure $ PreventDefault (dragEventToEvent e) (action NoOp))
                , HP.class_ (HH.ClassName "task-list")
                ]
                [ HH.h3_ [ HH.text st.laneName]
                , HH.p_
                  [ HH.button
                    [ HE.onClick (HE.input_ NewTask)]
                    [ HH.text "New Task"]
                  ]
                , HH.ul_ (map renderTask st.tasks)
                ]

              eval :: TaskListQuery ~> H.ParentDSL TaskList TaskListQuery TaskQuery TaskSlot TaskListMessage (Aff _)
              eval (AddTask task next) = do
                H.modify (\st -> st{tasks = st.tasks `snoc` task.taskId})
                void <<< H.query (TaskSlot task.taskId) $ H.request (SetTask task)
                pure $ next unit
              eval (NoOp next) = pure next
              eval (PreventDefault event q) = do
                liftEff $ preventDefault event
                eval q
              eval (NewTask next) = do
                newId <- liftEff genUUID
                liftAff <<< log $ "Adding task with ID: " <> show newId
                H.modify (\st -> st {tasks = st.tasks `snoc` newId})
                pure next
              eval (OnDrop event next) = do
                dataText <- liftEff <<< getData textPlain <<< dataTransfer $ event
                liftAff <<< log $ "dropped task: " <> dataText
                maybe (pure unit) (H.raise <<< NotifyTaskDropped) (parseUUID dataText)
                pure next
              eval (HandleTaskMessage taskId taskMsg next) = case taskMsg of
                (NotifyRemove) -> do
                  liftAff $ log "Removing task"
                  H.modify (\st -> st {tasks = (_ /= taskId) `filter` st.tasks})
                  pure next
                (NotifyIsDragging) -> do
                  liftAff <<< log $ "Dragging " <> show taskId
                  pure next
              eval (PopTask taskId next) = do
                liftAff <<< log $ "Popping task: " <> show taskId
                st <- H.get
                task <- H.query (TaskSlot taskId) $ H.request GetTask
                when (isJust task) do
                  H.modify (\st -> st{tasks = (_ /= taskId) `filter` st.tasks})
                pure $ next task

              renderTask :: TaskId -> H.ParentHTML _ _ _ (Aff _)
              renderTask taskId =
                HH.slot
                  (TaskSlot taskId)
                  (task {taskId, header: Nothing, content: Nothing})
                  unit
                  (HE.input (HandleTaskMessage taskId))

task :: Task -> H.Component HH.HTML TaskQuery Unit TaskMessage (Aff _)
task initialState = H.parentComponent
        { initialState: const initialState
        , render
        , eval
        , receiver: const Nothing
        }
        where
          render :: Task -> H.ParentHTML TaskQuery (SlamDownQuery String) SlamDownSlot (Aff _)
          render t = HH.li
            [ HP.class_ $ HH.ClassName "task"
            , HP.draggable true
            , HE.onDragStart (HE.input IsDragging)
            ]
            [
            -- [ HH.text $ show t.taskId
            -- , HH.br_
             HH.input
              [ HP.type_ HP.InputText
              , HP.class_ $ HH.ClassName "task-header"
              , HP.placeholder "Header"
              , HP.value $ fromMaybe "" t.header
              , HE.onValueChange (HE.input UpdateHeader)
              ]
            , HH.br_
            , HH.textarea
              [ HP.class_ $ HH.ClassName "task-content"
              , HP.cols 30
              , HP.rows 5
              , HP.placeholder "Content"
              , HP.value $ fromMaybe "" t.content
              , HE.onValueChange (HE.input UpdateContent)
              ]
            , HH.br_
            , HH.slot SlamDownSlot
              (slamDownComponent {formName: "slamdown", browserFeatures: {inputTypeSupported: const true}})
              unit
              (const Nothing)
            , HH.button
              [ HP.title "Remove task"
              , HE.onClick $ HE.input_ Remove
              , HP.class_ $ HH.ClassName "remove-task-button"
              ]
              [HH.text "☠"]
            ]

          eval :: TaskQuery ~> H.ParentDSL Task TaskQuery (SlamDownQuery String) SlamDownSlot TaskMessage (Aff _)
          eval (UpdateHeader h next) = do
            H.modify (\st -> st { header = pure h})
            liftAff $ log ("New header: " <> h)
            pure next
          eval (UpdateContent c next) = do
            H.modify (\st -> st { content = pure c})
            liftAff $ log ("New content: " <> c)
            case parseMd c of
              Left err -> do
                liftAff $ log "Parse failed"
                pure unit
              Right sd -> do
                void $ H.query SlamDownSlot $ SetDocument sd unit
            pure next
          eval (Remove next) = H.raise NotifyRemove *> pure next
          eval (IsDragging event next) = do
            H.raise NotifyIsDragging
            st <- H.get
            liftEff $ setData textPlain (show st.taskId) (dataTransfer event)
            pure next
          eval (GetTask next) = next <$> H.get
          eval (SetTask task next) = do
            H.put task
            pure $ next unit
