module Pux.Routing.Bob where

import Prelude
import Control.Monad.Aff (liftEff')
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Generic (class Generic)
import Data.Maybe (fromMaybe, Maybe(Nothing, Just))
import Data.String (dropWhile)
import Pux (onlyEffects, noEffects, Update)
import Pux.Html (Html, a)
import Pux.Html.Attributes (href)
import Pux.Html.Elements (Attribute)
import Pux.Html.Events (onClick)
import Pux.Router (sampleUrl)
import Routing.Bob (fromUrl, toUrl)
import Signal (Signal, (~>))

foreign import push :: forall eff. String -> Eff (dom :: DOM | eff) Unit

type Path = String

data RoutingError r
  = HistoryNotSupported r
  | SerializationError r

instance functorRoutingError :: Functor RoutingError where
  map f (HistoryNotSupported r) = HistoryNotSupported (f r)
  map f (SerializationError r) = SerializationError (f r)

data Action routesType
  = NotFound Path
  | Route routesType
  | Routed routesType
  | UrlChanged Path
  | RoutingError (RoutingError routesType)

instance functorActionWrapper :: Functor Action where
  map _ (NotFound p) = (NotFound p)
  map f (Route r) = Route (f r)
  map f (Routed r) = Routed (f r)
  map f (RoutingError e) = RoutingError (map f e)
  map _ (UrlChanged p) = (UrlChanged p)

update :: forall eff route state. (Generic route) =>
          Update state (Action route) (dom :: DOM | eff)
update (Route r) state =
  case toUrl r of
    Just url ->
      onlyEffects state [ liftEff' (push url) >>= (const $ pure (Routed r)) ]
    Nothing -> update (RoutingError (SerializationError r)) state
update (UrlChanged p) state =
  onlyEffects
    state
    [ case fromUrl (dropWhile (_ == '/') p) of
        Just r -> pure (Routed r)
        Nothing -> pure (NotFound p)
    ]
update _ state = noEffects state

link :: forall action route. (Generic route) =>
        ((Action route) -> action) ->
        route ->
        Array (Attribute action) ->
        Array (Html action) ->
        Html action
link fromRouterAction route attrs children =
  a attrs'
    children
 where
  attrs' =
    [ href (fromMaybe "" (toUrl route))
    , onClick (const (fromRouterAction (Route route)))
    ]

routeSignal' :: forall route eff. Eff (dom :: DOM | eff) (Signal (Action route))
routeSignal' = sampleUrl >>= (pure <<< (_ ~> UrlChanged))

routeSignal :: forall action route eff. ((Action route) -> action)
               -> Eff (dom :: DOM | eff) (Signal action)
routeSignal fromRouterAction = do
  s <- routeSignal'
  pure (s ~> fromRouterAction)
