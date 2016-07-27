module Pux.Routing.Bob.Frame where

import Prelude
import Pux.Router as Pux.Router
import Control.Monad.Aff (liftEff')
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Bifunctor (class Bifunctor)
import Data.Const (getConst, Const(Const))
import Data.Generic (class Generic)
import Data.Identity (Identity(Identity), runIdentity)
import Data.Maybe (Maybe(Nothing, Just))
import Data.String (dropWhile)
import Pux (Update, EffModel, onlyEffects)
import Pux.Html (Attribute, a)
import Pux.Html.Attributes (href)
import Pux.Html.Elements (Html)
import Pux.Html.Events (onClick)
import Pux.Routing.Bob (push)
import Routing.Bob (Router, fromUrl, toUrl)
import Signal (Signal, (~>))

type Path = String
data RoutingError = NotFound String

data RoutingRequest route = Route route | UrlChanged String
data RouterResponse route = Routed route | RoutingError RoutingError

data AppAction route action
  = RouterResponse (RouterResponse route)
  | RawAction action

data Action route action
  = RoutingRequest (RoutingRequest route)
  | AppAction (AppAction route action)

instance bifunctorAction :: Bifunctor Action where
  bimap f _ (RoutingRequest (Route r)) = RoutingRequest (Route (f r))
  bimap f _ (AppAction (RouterResponse (Routed r))) = routed (f r)
  bimap _ g (AppAction (RawAction a)) = rawAction (g a)
  bimap _ _ (RoutingRequest (UrlChanged p)) = RoutingRequest (UrlChanged p)
  bimap _ _ (AppAction (RouterResponse (RoutingError re))) =
    AppAction (RouterResponse (RoutingError re))

rawAction :: forall action route. action -> Action route action
rawAction = AppAction <<< RawAction

routed :: forall action route. route -> Action route action
routed = AppAction <<< RouterResponse <<< Routed

requestRoute :: forall action route. route -> Action route action
requestRoute = RoutingRequest <<< Route

-- types order similar as in Update alias
type ActionUpdate state route action eff =
  action -> state -> EffModel state (Action route action) (dom :: DOM | eff)

type RouteUpdate state route action eff =
  RouterResponse route -> state -> EffModel state (Action route action) (dom :: DOM | eff)

type State route state = { currentRoute :: route | state}


appUpdate :: forall action eff route state.
              Router route ->
              ActionUpdate (State route state) route action eff ->
              RouteUpdate (State route state) route action eff ->
              Update (State route state) (Action route action) (dom :: DOM | eff)
appUpdate router actionUpdate routeUpdate =
  update'
 where
  update' (AppAction (RawAction a)) state = actionUpdate a state
  update' (AppAction (RouterResponse r)) state = routeUpdate r state
  update' (RoutingRequest (Route r)) state =
    let url = toUrl router r
    in onlyEffects state [ liftEff' (push url) >>= (const <<< pure $ routed r) ]
  update' (RoutingRequest (UrlChanged p)) state =
    onlyEffects
      state
      [ case fromUrl router (dropWhile (_ == '/') p) of
          Just r -> pure <<< routed $ r
          Nothing -> pure <<< AppAction <<< RouterResponse <<< RoutingError <<< NotFound $ p
      ]

link :: forall action route.
        (Generic route) =>
        Router route ->
        route ->
        Array (Attribute (Action route action)) ->
        Array (Html (Action route action)) ->
        Html (Action route action)
link router route attrs children =
  let url = toUrl router route
      attrs' = [ href url
               , onClick (const (RoutingRequest <<< Route $ route))
               ] <> attrs
  in a attrs' children


sampleUrl :: forall action route eff.
             Eff (dom :: DOM | eff) (Signal (Action route action))
sampleUrl =
  Pux.Router.sampleUrl >>= pure <<< (_ ~> RoutingRequest <<< UrlChanged)

-- | I've no idea how to preserve simple Functor API,
-- | because inside view we want to call "href (toUrl router route)"
-- | and route has to be "updated" from the outside somehow
-- | in case of component reusability:
--    * we have to allow 
type View =
  forall action action' route route' state. (Generic route') =>
    Router route' ->
    (forall b.  (Bifunctor b) =>
      (b route action -> b route' action')) ->
    state ->
    Html (Action route' action')

-- sampleUrl' :: forall route eff. Eff (dom :: DOM | eff) (Signal (RouterAction route))
-- sampleUrl' = Pux.Router.sampleUrl >>= (pure <<< (_ ~> UrlChanged))

-- subroute :: forall action route. Lens subroute route -> 
-- makeSubroute subRouteLens subStateLens subroute =
--   subroute'
--  where
--   newSubroute = view lens newRoute
--   subroute' prevRoute newRoute state =
--     if view subRouteLens newRoute == view subRouteLens prevRoute
--       then
--         noEffects state
--       else
--         let result = subroute newSubroute
--         in { set subStateLens state result.state
--            , set su
-- 


