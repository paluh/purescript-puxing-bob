module Pux.Routing.Bob.Helpers where

import Prelude
import Data.Bifunctor (class Bifunctor)
import Data.Generic (class Generic)
import Pux.Html (a, Html, Attribute)
import Pux.Html.Attributes (href)
import Pux.Html.Events (onClick)
import Pux.Routing.Bob (RouterAction(Route))
import Routing.Bob (Router, toUrl)

data RouteOrAction routes actions
  = RouterAction (RouterAction routes)
  | RawAction actions

instance bifunctorRouteOrAction :: Bifunctor RouteOrAction where
  bimap f _ (RouterAction r) =
    RouterAction (f <$> r)
  bimap _ h (RawAction a) =
    RawAction (h a)

type RoutingContext route route' =
  { router :: Router route'
  , addContext :: (route -> route')
  }

type View route action state =
  forall route'. (Generic route') =>
  RoutingContext route route' ->
  state ->
  Html (RouteOrAction route' action)

link :: forall action route route'. (Generic route') =>
        RoutingContext route route' ->
        route ->
        Array (Attribute (RouteOrAction route' action)) ->
        Array (Html (RouteOrAction route' action)) ->
        Html (RouteOrAction route' action)
link routingContext route attrs children =
  a attrs'
    children
 where
  route' = routingContext.addContext route
  attrs' =
    [ href (toUrl routingContext.router route')
    , onClick (const <<< RouterAction <<< Route $ route')
    ] <> attrs

