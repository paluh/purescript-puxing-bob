# purescript-puxing-bob

Composable, bidiractional routing for Pux based on boomerang and generics.

## Caution!

This library is still proof of concept and is in testing phase...

## Overview

This module provides simple action type:

    -- Pux.Routing.Bob.purs

    type Path = String

    data RouterAction routesType
      -- these actions should be handled by your update
      = Routed routesType
      | RoutingError (RoutingError routesType)

      -- these actions should be passed to Pux.Routing.Bob.update
      | Route routesType
      | UrlChanged Path

It contains also `update` function which handles `Route routesType`, which should be send by you and `UrlChanged Path`, which is received through custom signal from the browser when url changes. In response to these actions this component creates `Routed r` or `RoutingError ...` actions which should be handled by your `update` function.

This module provides also signal constructor. This signal will handle direct url changes (encoded internally as `UrlChanged path` value) and through update it will generate appropriate response actions which should be handled by your `update`.

Additionally there is `Pux.Routing.Bob.Helpers` module which contains some helpers/proposition how to organize composable components which contains routing.

## Bidirectional routing

Bidirectional routing allows you to easily encode and decode urls to and from types. This library is based on __routing-bob__ and generates these parsers and serializers for free from data types which are instances of `Gneric` class (which can be derived autmagically in Pux :-)).
There is still one small caveat as __routing-bob__ is in early developement phase - serialization can fail for some types (__bob__ handles sum types, nested sum types, Ints and Strings now) and this failure is... silent - you can read it from the type:

    toUrl :: (Generic a) => a -> Maybe String

I hope that this `Maybe` will be history soon...

Of course there is complementary function provided:

    fromUrl :: (Generic a) => String -> Maybe String

With these two functions we can try to write simple `view` function as example...


## Usage

### Single component

Assume that we have component with three tabs (for example: `Profile`, `Inbox`, `Settings`) and we want to include currently active tab in url, so we need to define type which can encode this:

    import Data.Generic (class Generic)

    data Routes = Profile | Inbox | Settings
    derive instance genericRoutes :: Generic Routes

We have to preserve this information in component `State` as we want to use it in `view` function:

    type State =
        { activeTab :: Routes
        , nickName :: String
        ...
        }

Of course you have to include routing related actions also in your `Action` type:

    import Pux.Routing.Bob as Pux.Routing.Bob

    data Action
      = RouterAction Pux.Routing.Bob.RouterAction
      | SettingsFormAction SettingFormActions
      | ...

Now we can use routing in your view function:

    import Pux.Html (Html, ul, li)
    import Pux.Routing.Bob (link)
    import Pux.Routing.Bob as Pux.Routing.Bob

    view :: Html Action
    view state =
      ul
        []
        [ li [] [link fromRouterAction Profile [] [ text "profile" ]
        , li [] [link fromRouterAction Inbox [] [ text "inbox" ]
        , li [] [link fromRouterAction Settings [] [ text "settings" ]
        ]

     where
      fromRouterAction = RouterAction <<< Pux.Routing.Bob.RouterAction

There is little ugly secret which is hidden inside `link`  as serialization can fail sometimes and `href` will be empty then. Behaviour will be always correct 

The last step is to handle successful routing action in your update function

