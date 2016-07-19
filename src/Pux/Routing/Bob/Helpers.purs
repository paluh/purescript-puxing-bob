module Pux.Routing.Bob.Helpers where

import Prelude
import Pux.Routing.Bob as Pux.Routing.Bob
import Data.Bifunctor (class Bifunctor)
import Data.Generic (class Generic)
import Data.Maybe (fromMaybe)
import Pux (Update)
import Pux.Html (a, Html, Attribute)
import Pux.Html.Attributes (href)
import Pux.Html.Events (onClick)
import Pux.Routing.Bob (RouterAction(Routed, Route, RoutingError), RoutingError)
import Routing.Bob (toUrl)

data ActionsAndRoutes routes actions = RouterAction routes | RawAction actions

instance bifunctorActionsAndRoutes :: Bifunctor ActionsAndRoutes where
  bimap f _ (RouterAction r) =
    RouterAction (f r)
  bimap _ h (RawAction a) =
    RawAction (h a)

link :: forall actions routes. (Generic routes) =>
        routes ->
        Array (Attribute (ActionsAndRoutes (RouterAction routes) actions)) ->
        Array (Html (ActionsAndRoutes (RouterAction routes) actions)) ->
        Html (ActionsAndRoutes (RouterAction routes) actions)
link route attrs children =
  a attrs'
    children
 where
  attrs' =
    [ href (fromMaybe "" (toUrl route))
    , onClick (const (RouterAction (Route route)))
    ] <> attrs

link' addRoutingContext route attrs children =
  link (addRoutingContext route) attrs children

type View routes actions =
  forall routes'. (Generic routes') =>
  (routes -> routes') ->
  Html (ActionsAndRoutes routes' actions)

