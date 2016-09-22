module Pux.Routing.Bob where

import Prelude
import Pux.Router as Pux.Router
import Routing.Bob as Routing.Bob
import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Aff.Unsafe (unsafeInterleaveAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Location (pathname)
import DOM.HTML.Window (location)
import Data.Either (Either)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(Nothing, Just))
import Data.String (dropWhile)
import Pux (onlyEffects, noEffects, Update)
import Pux.Html (Html, a)
import Pux.Html.Attributes (href)
import Pux.Html.Elements (Attribute)
import Pux.Html.Events (onClick)
import Pux.Router (navigateTo)
import Routing.Bob (fromUrl, Router)
import Signal (Signal, (~>))

type Path = String

data RoutingError r
  = NotFound Path

instance functorRoutingError :: Functor RoutingError where
  map f (NotFound p) = NotFound p

-- | Base routing action type:
-- | - `RouteRequest r` - should be used to request url change
-- |    and should be passed to library `update` function.
-- | - `UrlChange s` is raised when browser location changes
-- |    should be passed to library `update` function too.
-- | - `Routed r` - is an response type which indicates route
-- |    change and should be handled in your application `update`
-- | - `RoutingError ...` - is error response action which
-- |    should be handled in your `update` too.
data RoutingAction routesType
  -- | handled by library `update` function
  = RouteRequest routesType
  | UrlChanged Path
  -- | handled by your `update` function
  | Routed routesType
  | RoutingError (RoutingError routesType)

instance functorRoutingActionWrapper :: Functor RoutingAction where
  map f (RouteRequest r) = RouteRequest (f r)
  map f (Routed r) = Routed (f r)
  map f (RoutingError e) = RoutingError (map f e)
  map _ (UrlChanged p) = (UrlChanged p)

toAbsoluteUrl :: forall route. Router route -> route -> String
toAbsoluteUrl = (("/" <> _) <$> _) <$> Routing.Bob.toUrl

fromAbsoluteUrl :: forall route. Router route -> String -> Maybe route
fromAbsoluteUrl r =  fromUrl r <<< dropWhile (_ == '/')

-- I'm not sure if this is really safe workaroud for double "err" label problem
push' :: forall eff. String -> Aff (dom :: DOM | eff) (Either Error Unit)
push' url = unsafeInterleaveAff (liftEff' (push url))

update :: forall eff route state. (Generic route) =>
          Router route ->
          Update state (RoutingAction route) (dom :: DOM | eff)
update router (RouteRequest r) state =
  let url = toAbsoluteUrl router r
  in onlyEffects state [ push' url >>= (const $ pure (Routed r)) ]
update router (UrlChanged p) state =
  onlyEffects
    state
    [ case fromAbsoluteUrl router p of
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
  let url = toAbsoluteUrl router route
      attrs' = [ href url
               , onClick (const (fromRoutingAction <<< RouteRequest $ route))
               ] <> attrs
  in a attrs' children

-- | Tries to parse current `window.location` value and if it is correct
-- | just returns parsed route value. In other case pushes `defaultRoute`
-- | onto history stack.
-- | Returns route which has been initialized.
setInitialRoute :: forall eff route. Router route -> route -> Eff (dom :: DOM | eff) route
setInitialRoute router defaultRoute = do
  maybeCurrRoute <- parseWindowLocation router
  case maybeCurrRoute of
    Just cr -> pure cr
    Nothing ->
      do
        pushRoute router defaultRoute
        pure defaultRoute

-- | push* and navigateTo* functions should be used only when initializing
-- | application. In other situations you should request url change through
-- | `RequestRoute r` action.

-- | Pushes history state (changes location) without signaling this event.
foreign import push :: forall eff. String -> Eff (dom :: DOM | eff) Unit

-- | Pushes route (changes location) without signaling this event.
pushRoute :: forall eff route.
              Router route ->
              route ->
              Eff (dom :: DOM | eff) Unit
pushRoute router route = push (toAbsoluteUrl router route)

-- | Pushes history state (changes location) signaling this event.
navigateToRoute :: forall eff route.
                    Router route ->
                    route ->
                    Eff (dom :: DOM | eff) Unit
navigateToRoute router route = navigateTo (toAbsoluteUrl router route)

-- | Tries to parse current window.location.pathname.
parseWindowLocation :: forall eff route.
                        Router route ->
                        Eff (dom :: DOM | eff) (Maybe route)
parseWindowLocation router =
  window >>= location >>= pathname >>= (pure <<< fromAbsoluteUrl router)
