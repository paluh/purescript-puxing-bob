module Pux.Routing.Bob where

import Prelude
import Control.Monad.Aff (liftEff')
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(Nothing, Just))
import Data.String (dropWhile)
import Pux (onlyEffects, noEffects, Update)
import Pux.Html (Html, a)
import Pux.Html.Attributes (href)
import Pux.Html.Elements (Attribute)
import Pux.Html.Events (onClick)
import Pux.Router as Pux.Router
import Pux.Routing.Bob (push)
import Routing.Bob as Routing.Bob
import Routing.Bob (Router, fromUrl)
import Signal (Signal, (~>))

foreign import push :: forall eff. String -> Eff (dom :: DOM | eff) Unit

type Path = String

data RoutingError r
  = HistoryNotSupported r
  | NotFound Path

instance functorRoutingError :: Functor RoutingError where
  map f (HistoryNotSupported r) = HistoryNotSupported (f r)
  map f (NotFound p) = NotFound p

data RoutingAction routesType
  -- handled by library `update` function
  = RouteRequest routesType
  | UrlChanged Path
  -- these should be handled by your `update` function
  | Routed routesType
  | RoutingError (RoutingError routesType)

instance functorRoutingActionWrapper :: Functor RoutingAction where
  map f (RouteRequest r) = RouteRequest (f r)
  map f (Routed r) = Routed (f r)
  map f (RoutingError e) = RoutingError (map f e)
  map _ (UrlChanged p) = (UrlChanged p)

toUrl :: forall route. Router route -> route -> String
toUrl = (("/" <> _) <$> _) <$> Routing.Bob.toUrl

update :: forall eff route state. (Generic route) =>
          Router route ->
          Update state (RoutingAction route) (dom :: DOM | eff)
update router (RouteRequest r) state =
  let url = toUrl router r
  in onlyEffects state [ liftEff' (push url) >>= (const $ pure (Routed r)) ]
update router (UrlChanged p) state =
  onlyEffects
    state
    [ case fromUrl router (dropWhile (_ == '/') p) of
        Just r -> pure <<< Routed $ r
        Nothing -> pure <<< RoutingError <<< NotFound $ p
    ]
update router _ state = noEffects state

sampleUrl :: forall route eff. Eff (dom :: DOM | eff) (Signal (RoutingAction route))
sampleUrl = Pux.Router.sampleUrl >>= (pure <<< (_ ~> UrlChanged))

sampleUrl' :: forall action route eff.
             (RoutingAction route -> action) ->
             Eff (dom :: DOM | eff) (Signal action)
sampleUrl' fromRoutingAction = do
  s <- sampleUrl
  pure (s ~> fromRoutingAction)

link :: forall action route. (Generic route) =>
        Router route ->
        (RoutingAction route -> action) ->
        route ->
        Array (Attribute action) ->
        Array (Html action) ->
        Html action
link router fromRoutingAction route attrs children =
  let url = toUrl router route
      attrs' = [ href url
               , onClick (const (fromRoutingAction <<< RouteRequest $ route))
               ] <> attrs
  in a attrs' children
