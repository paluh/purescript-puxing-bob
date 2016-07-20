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
import Routing.Bob (toUrl, Router, fromUrl)
import Signal (Signal, (~>))

foreign import push :: forall eff. String -> Eff (dom :: DOM | eff) Unit

type Path = String

data RoutingError r
  = HistoryNotSupported r
  | NotFound Path

instance functorRoutingError :: Functor RoutingError where
  map f (HistoryNotSupported r) = HistoryNotSupported (f r)
  map f (NotFound p) = NotFound p

data RouterAction routesType
  -- handled by library `update` function
  = Route routesType
  | UrlChanged Path
  -- these should be handled by your `update` function
  | Routed routesType
  | RoutingError (RoutingError routesType)

instance functorRouterActionWrapper :: Functor RouterAction where
  map f (Route r) = Route (f r)
  map f (Routed r) = Routed (f r)
  map f (RoutingError e) = RoutingError (map f e)
  map _ (UrlChanged p) = (UrlChanged p)

update :: forall eff route state. (Generic route) =>
          Router route ->
          Update state (RouterAction route) (dom :: DOM | eff)
update router (Route r) state =
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

sampleUrl' :: forall route eff. Eff (dom :: DOM | eff) (Signal (RouterAction route))
sampleUrl' = Pux.Router.sampleUrl >>= (pure <<< (_ ~> UrlChanged))

sampleUrl :: forall action route eff.
             (RouterAction route -> action) ->
             Eff (dom :: DOM | eff) (Signal action)
sampleUrl fromRouterAction = do
  s <- sampleUrl'
  pure (s ~> fromRouterAction)

link :: forall action route. (Generic route) =>
        Router route ->
        (RouterAction route -> action) ->
        route ->
        Array (Attribute action) ->
        Array (Html action) ->
        Html action
link router fromRouterAction route attrs children =
  let url = toUrl router route
      attrs' = [ href url
               , onClick (const (fromRouterAction <<< Route $ route))
               ] <> attrs
  in a attrs' children
