module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Pux (renderToDOM, start, noEffects, Update)
import Pux.Html (Html, div, ul, text, br, a, li)
import Pux.Html.Attributes (href, className)
import Pux.Html.Elements (p)
import Pux.Html.Events (onClick)
import Pux.Routing.Bob (RoutingAction(Routed, RoutingError))
import Pux.Routing.Bob.Component as Pux.Routing.Bob.Component
import Pux.Routing.Bob.Component (View, Action(RawAction, RoutingAction), link', sampleUrl)
import Routing.Bob as Routing.Bob
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

sideBarUpdate :: forall eff. Router SideBarRoute -> Update SideBarState SideBarAction (dom :: DOM | eff)
sideBarUpdate _ (RawAction _) state = noEffects state
sideBarUpdate _ (RoutingAction (Routed r)) state | r == Expanded = noEffects Expanded
                                                 | r == Minimized = noEffects Minimized
sideBarUpdate _ (RoutingAction (RoutingError _)) state = noEffects state
sideBarUpdate router (RoutingAction a) state =
  Pux.Routing.Bob.Component.update router a state

sideBarView :: View SideBarState SideBarRoute SideBarRawAction
sideBarView router mapAction state =
  p []
    if state == Expanded
      then
        [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
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
             (Router SideBarRoute) ->
             (Router AppRoute) ->
             Update AppState AppAction (dom :: DOM)
appUpdate mr _ _ (RawAction (MainWindowRawAction a)) state =
  let r = mainWindowUpdate mr (RawAction a) state.mainWindowState
  in
    { state: state { mainWindowState = r.state }
    , effects: (bimap mapRoute MainWindowRawAction <$> _) <$> r.effects
    }
 where
  mapRoute mainWindowRoute = AppRoute mainWindowRoute state.sideBarState
-- this branch could be avoided as sideBarUpdate doesn't perform anything
appUpdate _ sr _ (RawAction (SideBarRawAction a)) state =
  let r = sideBarUpdate sr (RawAction a) state.sideBarState
  in
    { state: state { sideBarState = r.state }
    , effects: (bimap mapRoute SideBarRawAction <$> _) <$> r.effects
    }
 where
  mapRoute sideBarRoute = AppRoute state.mainWindowState.activeTab sideBarRoute
appUpdate mRouter sRouter _
          (RoutingAction (Routed (AppRoute mainWindowRoute sideBarRoute)))
          state =
  let sr = sideBarUpdate sRouter (RoutingAction (Routed sideBarRoute)) state.sideBarState
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
appUpdate _ _ _ (RoutingAction (RoutingError _)) state = noEffects state
appUpdate _ _ aRouter (RoutingAction a) state =
  Pux.Routing.Bob.Component.update aRouter a state

appView :: Router AppRoute -> AppState -> Html AppAction
appView router state =
  div []
    [ mainWindowView router mapMainWindowAction state.mainWindowState
    , sideBarView router mapSideBarAction state.sideBarState
    ]
 where
  mapMainWindowRoute mainWindowRoute = AppRoute mainWindowRoute state.sideBarState
  mapMainWindowAction :: forall b. (Bifunctor b) =>
                         b MainWindowRoute MainWindowRawAction ->
                         b AppRoute AppRawAction
  mapMainWindowAction = bimap mapMainWindowRoute MainWindowRawAction

  mapSideBarRoute sideBarRoute = AppRoute state.mainWindowState.activeTab sideBarRoute
  mapSideBarAction :: forall b. (Bifunctor b) =>
                      b SideBarRoute SideBarRawAction ->
                      b AppRoute AppRawAction
  mapSideBarAction = bimap mapSideBarRoute SideBarRawAction

main :: Eff ( dom :: DOM , channel :: CHANNEL , err :: EXCEPTION ) Unit
main = do
  sampleUrlSignal <- sampleUrl
  let
    config = do
      -- this can be simplified with unsafePartial use
      -- on each component level, so all update functions
      -- would have appropriate router in scope
      mainWindowRouter <- Routing.Bob.router (Proxy :: Proxy MainWindowRoute)
      sideBarRouter <- Routing.Bob.router (Proxy :: Proxy SideBarRoute)
      appRouter <- Routing.Bob.router (Proxy :: Proxy AppRoute)
      let
        view = appView appRouter
        update = appUpdate mainWindowRouter sideBarRouter appRouter
      pure
        { update: update
        , view: view
        , inputs: [ sampleUrlSignal ]
        , initialState:
            { mainWindowState:
              { activeTab: Profile
              , profileCounter: 0
              , inboxCounter: 0
              , settingsCounter: 0
              }
            , sideBarState: Minimized
            }
        }
  case config of
    Just c ->
      do
        app <- start c
        renderToDOM "#app" app.html
    Nothing -> pure unit
