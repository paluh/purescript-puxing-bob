# purescript-puxing-bob

Bidiractional routing for Pux based on boomerang and generics. I'm exploring design space to find best way to incorporate routing into pux components...

## Caution!

This library is still proof of concept and is in testing phase...

## Bidirectional routing

Bidirectional routing allows you to easily encode and decode urls to and from types. All modules in this library are currently based on __routing-bob__ and generates these parsers and serializers for free from data types which are instances of `Generic` class (which can be derived autmagically in Pux :-)).
Here are the types of main routing functions:

```purescript

router :: (Generic a) => Proxy a -> Maybe (Router a)

toUrl :: Router a -> a -> String

fromUrl :: (Generic a) => String -> Maybe String

```

## Component.purs

This module implements simple component which can be embeded in your application and it will handle routing for you. This is not really composable approach, but is simple.

### Overview

This module provides simple action type:

```purescript

-- Pux.Routing.Bob.purs

type Path = String

data RouterAction routesType
  -- these actions should be handled by your update
  = Routed routesType
  | RoutingError (RoutingError routesType)

  -- these actions should be passed to Pux.Routing.Bob.update
  | Route routesType
  | UrlChanged Path

```

It contains also `update` function which handles `Route routesType` and `UrlChanged Path` actions. `Route routes` action should be created by you to request url change. `UrlChanged Path` is received through custom signal when url is modified (but not by application). In response to these actions this component creates `Routed r` or `RoutingError ...` actions which should be handled by your `update` function.

This module provides also signal constructor. This signal will handle direct url changes (encoded internally as `UrlChanged path` value) and through update it will generate appropriate response actions which should be handled by your `update`.

### Usage

Assume that we have application with three tabs (for example: `Profile`, `Inbox`, `Settings`) and we want to include currently active tab in url, so we need to define type which can encode this:

```purescript

import Data.Generic (class Generic)

data Route = Profile | Inbox | Settings
derive instance genericRoute :: Generic Route

```

We have to preserve this information in component `State` as we want to use it in `view` function:

```purescript

type State =
    { activeTab :: Route
    , nickName :: String
    ...
    }

```

Of course you have to include routing related actions also in your `Action` type:

```purescript

import Pux.Routing.Bob as Pux.Routing.Bob

data Action
  = RouterAction Pux.Routing.Bob.RouterAction
  | SettingsFormAction SettingFormActions
  | ...

```


You have to prepare router and pass it somehow to view and to this component update function (it such a simple scenario you can preserve it in a closure).
You also have to include signal (which will monitor direct url changes) into application configuration `inputs` table:

import Routing.Bob (router)

main = do
  ...

  let maybeRouter = router (Proxy :: Proxy Route)
  case maybeRouter of
    Just r -> do
      -- Setup routing signal and add it to `inputs` in your Pux application config):
      su <- sampleUrl RouterAction
      config = { inputs: [su]
               , update: update router
               , view: view router
               , state: ...
               }
    Nothing -> -- Maybe it's easier to just use... unsafePartial ;-)

Now we can use routing in your view function:

```purescript

import Pux.Html (Html, ul, li)
import Pux.Routing.Bob (link)
import Pux.Routing.Bob as Pux.Routing.Bob

view :: (Router Route) -> Html Action
view router state =
  ul
    []
    [ li [] [link' Profile [] [ text "profile" ]
    , li [] [link' Inbox [] [ text "inbox" ]
    , li [] [link' Settings [] [ text "settings" ]
    ]

 where
  link' = link router RouterAction

```

The last step is to handle routing action (responses from componnt) in your update function:

```purescript

import Pux.Routing.Bob as Pux.Routing.Bob

--handle successful location change
update router (RouterAction (Routed r)) = ...

-- handle unsuccessful routing action
update router (RouterAction (RoutingError e)) = ...

-- pass other routing related actions to the router
update router (RouterAction a) = Pux.Routing.Bob.Component.update router a

```


