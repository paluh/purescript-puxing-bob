module Pux.Routing.Bob.Helpers where

import Prelude
import Pux.Routing.Bob as Pux.Routing.Bob
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Bifunctor (bimap, class Bifunctor)
import Data.Bifunctor.Wrap (Wrap(Wrap), unwrap)
import Data.Generic (class Generic)
import Pux.Html (a, Html, Attribute)
import Pux.Html.Attributes (href)
import Pux.Html.Events (onClick)
import Pux.Routing.Bob (RouterAction(Route))
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

type HtmlRouteOrAction route rawAction = Html (RouteOrAction route rawAction)

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

type View route rawAction state =
  forall route'. (Generic route') =>
  RoutingContext route route' ->
  state ->
  HtmlRouteOrAction route' rawAction

link :: forall action route route'. (Generic route') =>
        RoutingContext route route' ->
        route ->
        Array (Attribute (RouteOrAction route action)) ->
        Array (HtmlRouteOrAction route action) ->
        HtmlRouteOrAction route' action
link (RoutingContext rc) route attrs children =
  a attrs' children'
 where
  route' = rc.addContext route
  attrs' =
    [ href (toUrl rc.router route')
    , onClick (const <<< RouterAction <<< Route $ route')
    ] <> ((bimap rc.addContext id <$> _) <$> attrs)
  children' = ((bimap rc.addContext id <$> _) <$> children)

newtype HtmlWrapper route rawAction = HtmlWrapper (Html (RouteOrAction route rawAction))

unwrapHtml :: forall route rawAction. HtmlWrapper route rawAction -> Html (RouteOrAction route rawAction)
unwrapHtml (HtmlWrapper html) = html

instance bifunctorHtmlWrapper :: Bifunctor HtmlWrapper where
  bimap f g (HtmlWrapper html) =
    HtmlWrapper (bimap f g <$> html)

instance functorHtmlWrapper :: Functor (HtmlWrapper r) where
  map f = unwrap <<< map f <<< Wrap

-- | Creates a signal with current location (`UrlChanged String`) which should
-- | be passed to `Pux.Routing.Bob.update`
sampleUrl :: forall action route eff.
               Eff (dom :: DOM | eff) (Signal (RouteOrAction route action))
sampleUrl = Pux.Routing.Bob.sampleUrl RouterAction
