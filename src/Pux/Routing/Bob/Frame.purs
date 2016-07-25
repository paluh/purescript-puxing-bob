module Pux.Routing.Bob.Frame where

import Prelude
import Control.Monad.Aff (liftEff')
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Bifunctor (class Bifunctor)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(Nothing, Just))
import Data.String (dropWhile)
import Pux (Update, EffModel, onlyEffects)
import Pux.Html (Attribute, a)
import Pux.Html.Attributes (href)
import Pux.Html.Elements (Html)
import Pux.Html.Events (onClick)
import Pux.Router as Pux.Router
import Pux.Routing.Bob (push)
import Routing.Bob (Router, fromUrl, toUrl)
import Signal (Signal, (~>))

type Path = String
data RoutingError = NotFound String

data RouterRequest route = Route route | UrlChanged String
data RouterResponse route = Routed route | RoutingError RoutingError

data AppAction route action
  = RouterResponse (RouterResponse route)
  | RawAction action

data Action route action
  = RouterRequest (RouterRequest route)
  | AppAction (AppAction route action)

instance bifunctorAction :: Bifunctor Action where
  bimap f _ (RouterRequest (Route r)) = RouterRequest (Route (f r))
  bimap f _ (AppAction (RouterResponse (Routed r))) = routed (f r)
  bimap _ g (AppAction (RawAction a)) = rawAction (g a)
  bimap _ _ (RouterRequest (UrlChanged p)) = RouterRequest (UrlChanged p)
  bimap _ _ (AppAction (RouterResponse (RoutingError re))) =
    AppAction (RouterResponse (RoutingError re))

rawAction :: forall action route. action -> Action route action
rawAction = AppAction <<< RawAction

routed :: forall action route. route -> Action route action
routed = AppAction <<< RouterResponse <<< Routed

requestRoute :: forall action route. route -> Action route action
requestRoute = RouterRequest <<< Route

-- types order similar as in Update alias
type ActionUpdate state route action eff =
  action -> state -> EffModel state (Action route action) (dom :: DOM | eff)

type RouteUpdate state route action eff =
  RouterResponse route -> state -> EffModel state (Action route action) (dom :: DOM | eff)

appUpdate :: forall action eff route state.
              Router route ->
              ActionUpdate state route action eff ->
              RouteUpdate state route action eff ->
              Update state (Action route action) (dom :: DOM | eff)
appUpdate router actionUpdate routeUpdate =
  update'
 where
  update' (AppAction (RawAction a)) state = actionUpdate a state
  update' (AppAction (RouterResponse r)) state = routeUpdate r state
  update' (RouterRequest (Route r)) state =
    let url = toUrl router r
    in onlyEffects state [ liftEff' (push url) >>= (const <<< pure $ routed r) ]
  update' (RouterRequest (UrlChanged p)) state =
    onlyEffects
      state
      [ case fromUrl router (dropWhile (_ == '/') p) of
          Just r -> pure <<< routed $ r
          Nothing -> pure <<< AppAction <<< RouterResponse <<< RoutingError <<< NotFound $ p
      ]

type State route state = { currentRoute :: route | state}

data LazyHtml a b =
  LazyHtml (a -> b) (forall c. (Generic c) => (a -> c) -> Html c)

runLazyHtml :: forall a b. (Generic b) => LazyHtml a b -> Html b
runLazyHtml (LazyHtml f h) = h f

instance functor :: Functor (LazyHtml a) where
  map f (LazyHtml g h) = LazyHtml (f <<< g) h

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
               , onClick (const (RouterRequest <<< Route $ route))
               ] <> attrs
  in a attrs' children


sampleUrl :: forall action route eff.
             Eff (dom :: DOM | eff) (Signal (Action route action))
sampleUrl =
  Pux.Router.sampleUrl >>= pure <<< (_ ~> RouterRequest <<< UrlChanged)

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


