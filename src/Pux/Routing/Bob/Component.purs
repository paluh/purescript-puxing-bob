module Pux.Routing.Bob.Component where

import Prelude
import Pux.Routing.Bob as Pux.Routing.Bob
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Bifunctor (class Bifunctor)
import Data.Bifunctor.Clown (Clown(Clown), runClown)
import Data.Generic (class Generic)
import Data.Identity (Identity(Identity), runIdentity)
import Pux (EffModel)
import Pux.Html (Attribute, Html)
import Pux.Routing.Bob (RoutingAction)
import Routing.Bob (Router)
import Signal (Signal)

data Action route action
  = RoutingAction (RoutingAction route)
  | RawAction action

instance bifunctorAction :: Bifunctor Action where
  bimap f _ (RoutingAction r) = RoutingAction (f <$> r)
  bimap _ g (RawAction a) = RawAction (g a)

-- | I know that this type is a bit complicated. Here is why:
-- |
-- | * we need to call `toAbsoluteUrl router route` in views to produce links href values,
-- |
-- | * we want to change this value from parent component, as it should capture
-- |   other parts of application url
-- |
-- | * we have to postpone this evaluation and allow to map it's arguments
-- |
-- | I've decided to add callback argument which operates on `Bifunctor b`
-- | to allow whole action (`Action route action`) mapping and single
-- | `route` value mapping too inside `view`.
type View state route action =
    forall route' action'. (Generic route') =>
    Router route' ->
    (forall b. (Bifunctor b) => b route action -> b route' action') ->
    state ->
    Html (Action route' action')

-- | Little helper which allows mapping over naked `route` value
-- | with function over `(Bifunctor b) => b route action`.
mapRoute :: forall action action' route route'.
            (forall b. (Bifunctor b) => b route action -> b route' action') ->
            route ->
            route'
mapRoute k = runIdentity <<< runClown <<< k <<< Clown <<< Identity

link :: forall action route. (Generic route) =>
        Router route ->
        route ->
        Array (Attribute (Action route action)) ->
        Array (Html (Action route action)) ->
        Html (Action route action)
link router route attrs children =
  Pux.Routing.Bob.link router RoutingAction route attrs children

link' :: forall action action' route route'. (Generic route') =>
         (forall b. (Bifunctor b) => (b route action -> b route' action')) ->
         Router route' ->
         route ->
         Array (Attribute (Action route' action')) ->
         Array (Html (Action route' action')) ->
         Html (Action route' action')
link' mapAction router route =
  link router (mapRoute mapAction route)

-- | Shortcut which wraps result of base `update` function
-- | (which operates on `Pux.Routing.Bob.RoutingAction r` values)
-- | into final `Action r a` value.
update :: forall action eff route state. (Generic route) =>
          Router route ->
          (RoutingAction route) ->
          state ->
          EffModel state (Action route action) (dom :: DOM | eff)
update router action state =
  let r = Pux.Routing.Bob.update router action state
  in  r { effects = (RoutingAction <$> _) <$> r.effects }

-- | Ready to use signal which should be added to your app config `inputs`
sampleUrl :: forall action eff route. Eff ( dom :: DOM | eff ) (Signal (Action action route))
sampleUrl = Pux.Routing.Bob.sampleUrl' RoutingAction
