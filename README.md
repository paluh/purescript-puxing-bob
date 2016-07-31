# purescript-puxing-bob

Bidiractional routing for Pux based on boomerangs and generics. I'm exploring design space to find best way to incorporate routing into pux components...

## Caution!

This library is still just a proof of concept and is in testing phase...

## Bidirectional routing

Bidirectional routing allows you to generate urls encoders and decoders from types. All modules in this library are currently based on [purescript-routing-bob](https://github.com/paluh/purescript-routing-bob). Parsers and serializers are generated from data types which are instances of `class Generic` (which can be derived autmagically in PureScript :-)).
Here is routing API which is base for this library:

```purescript

router :: forall a. (Generic a) => Proxy a -> Maybe (Router a)

toUrl :: forall a. Router a -> a -> String

fromUrl :: forall a. Router a -> String -> Maybe a

```

## Pux.Routing.Bob

This module implements simple approach to routing and can be embeded in your application, so it will handle routing for you. It's architecture is not really composable, but is simple and is base for an extended, more composable implementation, so let's start with it.

### Overview

This module provides simple action type:

```purescript

type Path = String

data RouterAction routesType
  -- these actions should be handled by your update
  = Routed routesType
  | RoutingError (RoutingError routesType)

  -- these actions should be passed to Pux.Routing.Bob.update
  | Route routesType
  | UrlChanged Path

```

It contains also `update` function which handles `Route routesType` and `UrlChanged Path` actions. `Route routesType` action should be created by you to request url change. `UrlChanged Path` is received through custom signal when url is modified (but not by application). In response to these actions this component creates `Routed r` or `RoutingError ...` actions which should be handled by your `update` function.

This module provides also signal constructor. This signal will handle direct url changes (encoded internally as `UrlChanged path` value) and through `update` it will generate appropriate response actions which should be handled by your `update`.

So the only thing you should care about, is to create proper `Route routesType` actions and handle `Routed routesType` and `RoutingError ...` in your `update`.

### Usage

You can find full and working code of this example in [examples/single-component-routing](https://github.com/paluh/purescript-puxing-bob/tree/master/examples/single-component-routing).

Assume that we have application with three tabs (for example: `Profile`, `Inbox`, `Settings`) and we want to include currently active tab in url, so we need to define type which can encode this:

```purescript

import Data.Generic (class Generic)

data Route
  = Profile
  | Inbox
  | Settings
derive instance genericRoute :: Generic Route

```

We have to preserve this information in component `State` as we want to use it in `view` function:

```purescript

type State =
  { activeTab :: Route
  ...
  }

```

Of course you have to include routing related actions also in your `Action` type:

```purescript

import Pux.Routing.Bob as Pux.Routing.Bob

data Action
  = RoutingAction (Pux.Routing.Bob.RoutingAction Route)
  | ...

```

You have to prepare router and pass it somehow to `view` and to this component `update` function (or you can [cheat](#cheating), a bit ;-)).  You also have to include signal (which will monitor direct url changes) into application configuration `inputs` list:

```purescript

import Routing.Bob (router)

main = do
  ...

  let maybeRouter = router (Proxy :: Proxy Route)
  su <- sampleUrl RoutingAction
  case maybeRouter of
    Just r -> do
      -- Setup routing signal and add it to `inputs` in your Pux application config:
      config = { inputs: [su]
               , update: update router
               , view: view router
               , initialState: ...
               }
    Nothing -> -- Maybe it's easier to just use... unsafePartial ;-)

```

Now we can use routing in `view` function:

```purescript

import Pux.Html (Html, ul, li)
import Pux.Routing.Bob (link)

view :: (Router Route) -> Html Action
view router state =
  ul
    []
    [ menuItem Profile "profile"
    , menuItem Inbox "inbox"
    , menuItem Settings "settings"
    ]

 where
  menuItem route label =
    li
      if state.activeTab == route
        then [className "active"]
        else []
      [ link router RoutingAction route
        []
        [ text label ]
      ]

```

The last step is to handle routing action (responses from component) in your update function:

```purescript

import Pux.Routing.Bob as Pux.Routing.Bob

--handle successful location change
update router (RoutingAction (Routed r)) = ...

-- handle unsuccessful routing action
update router (RoutingAction (RoutingError e)) = ...

-- pass other routing related actions to the router
update router (RoutingAction a) = Pux.Routing.Bob.update router a

```

## Pux.Routing.Bob.Component

This module implements basic approach to composable component implementation. It is still just a proof of concept. You can find full and working example implementation of routing composition here: [examples/multiple-components-routing](https://github.com/paluh/purescript-puxing-bob/tree/master/examples/multiple-components-routing).

### Action type

There is `Action` type provided which is based on the `RoutingAction` from base library module. This action type is parametrized by two type variables - one for route encoding and one for "normal" action. It seems that "regular" actions composition is done through sum of subcomponents actions, but composition of routes is a product of subroutes. The `Action` type, thanks to it's clear separation between `route` and `action`, facilitates usage of these two strategies of composition.
It is possible to `map` over an `Action` value, because `Action` is `Bifunctor` instance, so you can turn `Action subcomponentRoute subcomponentRawAction` into `Action appRoute appRawAction`.

```purescript

import Pux.Routing.Bob (RoutingAction)

data Action route action
  = RoutingAction (RoutingAction route)
  | RawAction action

```

### View type

I think that the ugliest part of this `API` is the `View` type. It is a bit complicated and completly removes possiblity to work with `Html action` through it's `Functor` instance.


```purescript

type View state route action =
    forall route' action'. (Generic route') =>
    Router route' ->
    (forall b. (Bifunctor b) => b route action -> b route' action') ->
    state ->
    Html (Action route' action')

```

Here is why we need such a complicated `View` type:

* we want to perform `toUrl router route` (for example when generating `href` values),

* this value has to somehow capture external (parent component) routing context, because final url should contain other components routes,

* I don't think we can pospone this evaluation in any other way than through callback (maybe there is some nice abstraction??? - we need to map over bare `Route`, when generating `href` values, and and over `Action Route RawAction` values)

* this callback should be used in place of `Functor`'s `map` and should operate on any `Bifunctor`

So final component view could look like this:

``` purescript
import Pux.Routing.Bob.Component (View, link')

componentView :: View ComponentState ComponentRoute ComponentAction
componentView router mapAction state =
  div []
    -- link which causes route change
    [ link' router mapAction SomeRoute [] [ text "route" ]
    -- link which raises action
    , a [ href "#", onClick $ const (mapAction (RawAction ClickAction))] [ text "action" ]
    ]
```

From parent component we can use this subview with `bimap` over route and action type - please check `appView` from 'examples/multiple-components/src/Main.purs'.

### Cheating

I've found that a bit of cheating can simplify subcomponents API and integration a lot. By cheating I mean usage of `unsafePartial` in places where routers are generated:

``` purescript

import Data.Maybe (Maybe(Just))
import Partial.Unsafe (unsafePartial)
import Routing.Bob (router, Router)
import Type.Proxy (Proxy(Proxy))

componentRouter :: Router MyRouteType
componentRouter = unsafePartial (case (router (Proxy :: Proxy MyRouteType)) of Just r -> r)

```

As `MyRouteType` is not going to change during application lifetime (and it should be tested before production build anyway ;-)), it is "safe" to use this pattern to simplify subcomponents API and drop router parameter from your `update` functions.
