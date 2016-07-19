module Pux.Routing.Bob.Helpers where

import Prelude
import Data.Bifunctor (class Bifunctor)
import Data.Generic (class Generic)
import Data.Maybe (fromMaybe)
import Pux.Html (a, Html, Attribute)
import Pux.Html.Attributes (href)
import Pux.Html.Events (onClick)
import Pux.Routing.Bob (RouterAction(Route))
import Routing.Bob (toUrl)

data RouteOrAction routes actions
  = RouterAction (RouterAction routes)
  | RawAction actions

instance bifunctorRouteOrAction :: Bifunctor RouteOrAction where
  bimap f _ (RouterAction r) =
    RouterAction (f <$> r)
  bimap _ h (RawAction a) =
    RawAction (h a)

link :: forall action route. (Generic route) =>
        route ->
        Array (Attribute (RouteOrAction route action)) ->
        Array (Html (RouteOrAction route action)) ->
        Html (RouteOrAction route action)
link route attrs children =
  a attrs'
    children
 where
  attrs' =
    [ href (fromMaybe "#" (toUrl route))
    , onClick (const (RouterAction (Route route)))
    ] <> attrs

type View route action =
  forall route'. (Generic route') =>
  (route -> route') ->
  Html (RouteOrAction route' action)

link' :: forall action route route'. (Generic route') =>
         (route -> route') ->
         route ->
         Array (Attribute (RouteOrAction route' action)) ->
         Array (Html (RouteOrAction route' action)) ->
         Html (RouteOrAction route' action)
link' addRoutingContext route attrs children =
  link (addRoutingContext route) attrs children

