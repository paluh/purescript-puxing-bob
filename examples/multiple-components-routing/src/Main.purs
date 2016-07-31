module Main where

import Pux.Routing.Bob.Component as Pux.Routing.Bob.Component
import Routing.Bob as Routing.Bob
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Partial.Unsafe (unsafePartial)
import Pux (renderToDOM, start, noEffects, Update)
import Pux.Html (Html, div, ul, text, br, a, li)
import Pux.Html.Attributes (href, className)
import Pux.Html.Elements (p)
import Pux.Html.Events (onClick)
import Pux.Routing.Bob (RoutingAction(RoutingError, Routed), setInitialRoute)
import Pux.Routing.Bob.Component (View, Action(RawAction, RoutingAction), link', sampleUrl)
import Routing.Bob (Router)
import Signal.Channel (CHANNEL)
import Type.Proxy (Proxy(Proxy))
import Prelude hiding (div)

-- Main window component

data MainWindowRoute
  = Profile
  | Inbox
  | Settings
derive instance genericMainWindowRoute :: Generic MainWindowRoute
derive instance eqMainWindowRoute :: Eq MainWindowRoute

data MainWindowRawAction
  = ProfileClick
  | InboxClick
  | SettingsClick

type MainWindowState =
  { activeTab :: MainWindowRoute
  -- clicks counters - just to include other state and actions
  , profileCounter :: Int
  , inboxCounter :: Int
  , settingsCounter :: Int
  }

type MainWindowAction = Action MainWindowRoute MainWindowRawAction

mainWindowUpdate :: (Router MainWindowRoute) -> Update MainWindowState MainWindowAction (dom :: DOM)
mainWindowUpdate _ (RawAction ProfileClick) state =
  noEffects $ state { profileCounter = state.profileCounter + 1 }
mainWindowUpdate _ (RawAction InboxClick) state =
  noEffects $ state { inboxCounter = state.inboxCounter + 1 }
mainWindowUpdate _ (RawAction SettingsClick) state =
  noEffects $ state { settingsCounter = state.settingsCounter + 1 }
mainWindowUpdate _ (RoutingAction (Routed r)) state =
  noEffects $ state { activeTab = r }
mainWindowUpdate _ (RoutingAction (RoutingError _)) state = noEffects state
mainWindowUpdate router (RoutingAction a) state =
  Pux.Routing.Bob.Component.update router a state

mainWindowView :: View MainWindowState MainWindowRoute MainWindowRawAction
mainWindowView router mapAction state =
  div []
    [ ul []
        [ tabHeader Profile "profile"
        , tabHeader Inbox "inbox"
        , tabHeader Settings "settings"
        ]
    , div []
        case state.activeTab of
          Profile -> [ tabBody (mapAction (RawAction ProfileClick)) state.profileCounter "profile" ]
          Inbox -> [ tabBody (mapAction (RawAction InboxClick)) state.inboxCounter "inbox" ]
          Settings -> [ tabBody (mapAction (RawAction SettingsClick)) state.settingsCounter "settings" ]
    ]
 where
  link'' = link' mapAction router
  tabHeader route label =
    li
      (if state.activeTab == route
        then [className "active"]
        else [])
      [ link''
          route
          []
          [ text label ]
      ]
  tabBody action counter label =
    a [ href "#", onClick $ const action]
      [ text (label <> " action"), br [] [], text <<< show $ counter ]

-- Sidebar component

data SideBarRoute
  = Expanded
  | Minimized
derive instance genericSideBarRoute :: Generic SideBarRoute
derive instance eqSideBarRoute :: Eq SideBarRoute

type SideBarState = SideBarRoute

type SideBarRawAction = Unit

type SideBarAction = Action SideBarRoute SideBarRawAction

-- | This is how you can cheat to cleanup your subcomponent
-- | `update` API a bit and make subcomponent more self-contained.
-- | I've left this example here, side by side with default,
-- | safe implementation of `Router MainWidowRoute` construction,
-- | just for comparison.
sideBarRouter :: Router SideBarRoute
sideBarRouter =
  unsafePartial
    (case (Routing.Bob.router (Proxy :: Proxy SideBarRoute)) of Just r -> r)

sideBarUpdate :: forall eff. Update SideBarState SideBarAction (dom :: DOM | eff)
sideBarUpdate (RawAction _) state = noEffects state
sideBarUpdate (RoutingAction (Routed r)) state | r == Expanded = noEffects Expanded
                                               | r == Minimized = noEffects Minimized
sideBarUpdate (RoutingAction (RoutingError _)) state = noEffects state
sideBarUpdate (RoutingAction a) state =
  Pux.Routing.Bob.Component.update sideBarRouter a state

sideBarView :: View SideBarState SideBarRoute SideBarRawAction
sideBarView router mapAction state =
  p []
    if state == Expanded
      then
        [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
        , br [] []
        , link'' Minimized [] [ text "hide"]
        ]
      else
        [ text "Lorem ipsum..."
        , link'' Expanded [] [ text "show more" ]
        ]
 where
  link'' = link' mapAction router

-- Application

data AppRoute =
  AppRoute MainWindowRoute SideBarRoute
derive instance genericAppRoute :: Generic AppRoute

getMainWindowRoute :: AppRoute -> MainWindowRoute
getMainWindowRoute (AppRoute m _) = m

getSideBarRoute :: AppRoute -> SideBarRoute
getSideBarRoute (AppRoute _ s) = s

type AppState =
  { mainWindowState :: MainWindowState
  , sideBarState :: SideBarState
  }

data AppRawAction
  = MainWindowRawAction MainWindowRawAction
  | SideBarRawAction SideBarRawAction

type AppAction = Action AppRoute AppRawAction

-- To be honest I'm using: unsafePartial (router RouteType)
-- for Router generation when it's tested, because route type
-- doesn't change over runtime. This simplifies API
-- a lot - you can avoid all this routers passing clutter...
appUpdate :: (Router MainWindowRoute) ->
             (Router AppRoute) ->
             Update AppState AppAction (dom :: DOM)
appUpdate mr _ (RawAction (MainWindowRawAction a)) state =
  let r = mainWindowUpdate mr (RawAction a) state.mainWindowState
  in
    { state: state { mainWindowState = r.state }
    , effects: (bimap mapRoute MainWindowRawAction <$> _) <$> r.effects
    }
 where
  mapRoute mainWindowRoute = AppRoute mainWindowRoute state.sideBarState
-- this branch could be avoided as sideBarUpdate doesn't perform anything
appUpdate _ _ (RawAction (SideBarRawAction a)) state =
  let r = sideBarUpdate (RawAction a) state.sideBarState
  in
    { state: state { sideBarState = r.state }
    , effects: (bimap mapRoute SideBarRawAction <$> _) <$> r.effects
    }
 where
  mapRoute sideBarRoute = AppRoute state.mainWindowState.activeTab sideBarRoute
appUpdate mRouter
          _
          (RoutingAction (Routed (AppRoute mainWindowRoute sideBarRoute)))
          state =
  let sr = sideBarUpdate (RoutingAction (Routed sideBarRoute)) state.sideBarState
      mr = mainWindowUpdate mRouter (RoutingAction (Routed mainWindowRoute)) state.mainWindowState
      mapSideBarRoute sideBarRoute = AppRoute mr.state.activeTab sideBarRoute
      mapMainWindowRoute mainWindowRoute = AppRoute mainWindowRoute sr.state
  in
    { state: state { sideBarState = sr.state
                   , mainWindowState = mr.state
                   }
    , effects: ((bimap mapSideBarRoute SideBarRawAction <$> _) <$> sr.effects) <>
               ((bimap mapMainWindowRoute MainWindowRawAction <$> _) <$> mr.effects)
    }
appUpdate _ _ (RoutingAction (RoutingError _)) state = noEffects state
appUpdate _ aRouter (RoutingAction a) state =
  Pux.Routing.Bob.Component.update aRouter a state

appView :: Router AppRoute -> AppState -> Html AppAction
appView router state =
  div []
    [ mainWindowView
        router
        (bimap (\r -> AppRoute r state.sideBarState) MainWindowRawAction)
        state.mainWindowState
    , sideBarView router mapSideBarAction state.sideBarState
    ]
 where
  -- Helper with type signature, which is necessary in this case
  mapSideBarAction :: forall b. (Bifunctor b) =>
                      b SideBarRoute SideBarRawAction ->
                      b AppRoute AppRawAction
  mapSideBarAction = bimap (\r -> AppRoute state.mainWindowState.activeTab r) SideBarRawAction


main :: Eff ( dom :: DOM , channel :: CHANNEL , err :: EXCEPTION ) Unit
main = do
  sampleUrlSignal <- sampleUrl
  let
    routers = do
      mainWindowRouter <- Routing.Bob.router (Proxy :: Proxy MainWindowRoute)
      appRouter <- Routing.Bob.router (Proxy :: Proxy AppRoute)
      pure
        { appRouter: appRouter
        , mainWindowRouter: mainWindowRouter
        }

  case routers of
    Just rs ->
      do
        currRoute <- setInitialRoute rs.appRouter (AppRoute Profile Minimized)
        let
          view = appView rs.appRouter
          update = appUpdate rs.mainWindowRouter rs.appRouter
          initialState =
            { mainWindowState:
              { activeTab: getMainWindowRoute currRoute
              , profileCounter: 0
              , inboxCounter: 0
              , settingsCounter: 0
              }
            , sideBarState: getSideBarRoute currRoute
            }
          config =
            { update: update
            , view: view
            , inputs: [ sampleUrlSignal ]
            , initialState: initialState
            }
        app <- start config
        renderToDOM "#app" app.html
    Nothing -> pure unit
