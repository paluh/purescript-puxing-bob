# purescript-puxing-bob

Bidiractional routing for Pux based on boomerangs and generics. I'm exploring design space to find best way to incorporate routing into pux components...

## Caution!

This library is still proof of concept and is in testing phase...

## Bidirectional routing

Bidirectional routing allows you to easily encode and decode urls to and from types. All modules in this library are currently based on __purescript-routing-bob__ and generates these parsers and serializers for free from data types which are instances of `class Generic` (which can be derived autmagically in Pux :-)).
Here are the types of main routing functions:

```purescript

router :: (Generic a) => Proxy a -> Maybe (Router a)

toUrl :: Router a -> a -> String

fromUrl :: (Generic a) => String -> Maybe String

```

## Pux.Routing.Bob

This module implements simple approach for routing, which can be embeded in your application and it will handle routing for you. This is not really composable approach, but it is simple.

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

So the only think you should care is to create proper `Route routesType` actions and handle `Routed routesType` and `RoutingError ...` actions.

### Usage

You can find full and working code of example in examples/single-component-routing

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


You have to prepare router and pass it somehow to view and to this component update function (it such a simple scenario you can preserve it in a closure).
You also have to include signal (which will monitor direct url changes) into application configuration `inputs` table:

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

Now we can use routing in your view function:

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

This module implements basic approach to composable component implementation. It is still just proof of concept.

### Action type

There is `Action` type provided which is based on `RoutingAction` from base library module. This action type is parametrized by two type variables - one for route encoding type and one for normal action. It seems that regular actions composition is done through sum of subcomponents actions, but composition of routes is just a product. `Action` type, thanks to clear separation between these two types, allows to separate these composition.
Mapping over `Action` is still possible thanks to `Bifunctor` instance.

```purescript

import Pux.Routing.Bob (RoutingAction)

data Action route action
  = RoutingAction (RoutingAction route)
  | RawAction action

```

### View type

I think that the ugliest part of this `API` is `View` type. It is a bit complicated and completly removes possiblity to work with `Html action` through it's `Functor` instance.


```purescript

type View state route action =
    forall route' action'. (Generic route') =>
    Router route' ->
    (forall b. (Bifunctor b) => b route action -> b route' action') ->
    state ->
    Html (Action route' action')

```

Here is why we need such a complicated `View` type:

* we want to perform `toUrl router route` (for example when generating `href` values)

* this value has to somehow capture external (parent component) routing context, because final url should contain other components routes

* I don't think we can pospone this evaluation in any other way than through callback, but maybe there is some nice abstraction...

* this callback should be used in place of `Functor`'s `map` and should operate on any `Bifunctor`

So final component view could look like this:

``` purescript
import Pux.Routing.Bob.Component (View, link')

componentView :: View ComponentState ComponentRoute ComponentAction
componentView router mapAction state =
  div []
    -- link which causes route change
    [ link' router mapAction SomeRoute [] [ text "route ]
    -- link which raises action
    , a [ href "#", onClick $ const (mapAction ClickAction)] [ text "action" ]
    ]
```

From parent component we can use this subview with `bimap` over route and action type - please check `appView` from 'examples/multiple-components/src/Main.purs'.
