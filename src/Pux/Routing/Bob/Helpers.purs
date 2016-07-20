module Pux.Routing.Bob.Helpers where

import Prelude
import Pux.Html as Pux.Html
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Bifunctor (class Bifunctor)
import Data.Generic (class Generic)
import Pux.Html (a, Attribute)
import Pux.Html.Attributes (href)
import Pux.Html.Events (onClick)
import Pux.Routing.Bob (RouterAction(Route))
import Pux.Routing.Bob as Pux.Routing.Bob
import Routing.Bob (Router, toUrl)
import Signal (Signal, (~>))

data RouteOrAction routes actions
  = RouterAction (RouterAction routes)
  | RawAction actions

instance bifunctorRouteOrAction :: Bifunctor RouteOrAction where
  bimap f _ (RouterAction r) =
    RouterAction (f <$> r)
  bimap _ h (RawAction a) =
    RawAction (h a)

newtype RoutingContext route route' =
  RoutingContext
    { router :: Router route'
    , addContext :: (route -> route')
    }

instance semigroupoidRoutingContext :: Semigroupoid RoutingContext where
  compose (RoutingContext { router: r, addContext: c })
          (RoutingContext { router: r', addContext: c' }) =
    RoutingContext { router: r, addContext: c <<< c' }

routingContext :: forall route route'. (Generic route') =>
                  Router route' ->
                  (route -> route') ->
                  RoutingContext route route'
routingContext router addContext = RoutingContext { router, addContext }

idRoutingContext :: forall route.
                    Router route ->
                    RoutingContext route route
idRoutingContext router = RoutingContext { router, addContext: id }

type Html route rawAction = Pux.Html.Html (RouteOrAction route rawAction)

type View route rawAction state =
  forall route'. (Generic route') =>
  RoutingContext route route' ->
  state ->
  Html route' rawAction

link :: forall action route route'. (Generic route') =>
        RoutingContext route route' ->
        route ->
        Array (Attribute (RouteOrAction route action)) ->
        Array (Html route action) ->
        Html route action
link (RoutingContext rc) route attrs children =
  a attrs'
    children
 where
  route' = rc.addContext route
  attrs' =
    [ href (toUrl rc.router route')
    , onClick (const <<< RouterAction <<< Route $ route)
    ] <> attrs

-- | Creates a signal with current location (`UrlChanged String`) which should
-- | be passed to `Pux.Routing.Bob.update`
sampleUrl :: forall action route eff.
               Eff (dom :: DOM | eff) (Signal (RouteOrAction route action))
sampleUrl = Pux.Routing.Bob.sampleUrl RouterAction
