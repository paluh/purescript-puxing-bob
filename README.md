# purescript-puxing-bob

Bidiractional routing for Pux based on boomerang and generics. Unfortunatelly you have to still make a lot of manual plumbing to make it composable...

## Caution!

This library is still proof of concept and is in testing phase...

## Overview

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

It contains also `update` function which handles `Route routesType`, which should be send by you and `UrlChanged Path`, which is received through custom signal from the browser when url changes. In response to these actions this component creates `Routed r` or `RoutingError ...` actions which should be handled by your `update` function.

This module provides also signal constructor. This signal will handle direct url changes (encoded internally as `UrlChanged path` value) and through update it will generate appropriate response actions which should be handled by your `update`.

## Bidirectional routing

Bidirectional routing allows you to easily encode and decode urls to and from types. This library is based on __routing-bob__ and generates these parsers and serializers for free from data types which are instances of `Gneric` class (which can be derived autmagically in Pux :-)).
Here are the types of main routing functions:

    ```purescript
    router :: (Generic a) => Proxy a -> Maybe (Router a)

    toUrl :: Router a -> a -> String

    fromUrl :: (Generic a) => String -> Maybe String
    ```

There is still one small caveat, as __routing-bob__ is in early developement phase, router construction can fail for some types - __bob__ handles sum types, nested sum types, `Int`s and `String`s now, but I'm going to extend this set soon.


## Usage

This manual is still work in progress as API fluctuates a lot ;-)

Assume that we have component with three tabs (for example: `Profile`, `Inbox`, `Settings`) and we want to include currently active tab in url, so we need to define type which can encode this:

    import Data.Generic (class Generic)

    data Routes = Profile | Inbox | Settings
    derive instance genericRoutes :: Generic Routes

We have to preserve this information in component `State` as we want to use it in `view` function:

    type State =
        { activeTab :: Routes
        , router :: Router Routes
        , nickName :: String
        ...
        }

Of course you have to include routing related actions also in your `Action` type:

    import Pux.Routing.Bob as Pux.Routing.Bob

    data Action
      = RouterAction Pux.Routing.Bob.RouterAction
      | SettingsFormAction SettingFormActions
      | ...

You have to prepare router and pass it somehow to view and to this component update function:

    maybeRouter = router (Proxy :: Proxy Routes)

Now we can use routing in your view function:

    import Pux.Html (Html, ul, li)
    import Pux.Routing.Bob (link)
    import Pux.Routing.Bob as Pux.Routing.Bob

    view :: (Router Routes) -> Html Action
    view router state =
      ul
        []
        [ li [] [link' Profile [] [ text "profile" ]
        , li [] [link' Inbox [] [ text "inbox" ]
        , li [] [link' Settings [] [ text "settings" ]
        ]

     where
      link' = link router RouterAction
      fromRouterAction = RouterAction <<< Pux.Routing.Bob.RouterAction

The last step is to handle successful routing action in your update function:

    ```purescript
    --handle successful location change
    update router (RouterAction (Routed r)) = ...
    -- handle unsuccessful routing action
    update router (RouterAction (RoutingError e)) = ...
    -- pass other routing related actions to the router
    update router (RouterAction a) = Pux.Routing.Bob.update router a
    ```

