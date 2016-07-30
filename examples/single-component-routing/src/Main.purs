module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import Data.Generic (gEq, class Generic)
import Data.Maybe (Maybe(Nothing, Just))
import Pux (renderToDOM, start, noEffects, Update)
import Pux.Html (br, div, a, text, li, ul, Html)
import Pux.Html.Attributes (href, className)
import Pux.Html.Events (onClick)
import Pux.Routing.Bob (link, RoutingAction(Routed, RoutingError), sampleUrl')
import Routing.Bob (router, Router)
import Signal.Channel (CHANNEL)
import Type.Proxy (Proxy(Proxy))
import Prelude hiding (div)
import Pux.Routing.Bob as Pux.Routing.Bob

data Route
  = Profile
  | Inbox
  | Settings
derive instance genericRoute :: Generic Route
derive instance eqRoute :: Eq Route

type State =
  { activeTab :: Route
  -- clicks counters - just to include other state and actions
  , profileCounter :: Int
  , inboxCounter :: Int
  , settingsCounter :: Int
  }

data Action
  = RoutingAction (RoutingAction Route)
  -- for simplicity and for demonstration purposes
  -- let's use really trivial appliaction actions
  | ProfileAction
  | InboxAction
  | SettingsAction

view :: (Router Route) -> State -> Html Action
view router state =
  div []
    [ ul []
        [ tabHeader Profile "profile"
        , tabHeader Inbox "inbox"
        , tabHeader Settings "settings"
        ]
    , div []
        case state.activeTab of
          Profile -> [ tabBody ProfileAction state.profileCounter "profile" ]
          Inbox -> [ tabBody InboxAction state.inboxCounter "inbox" ]
          Settings -> [ tabBody SettingsAction state.settingsCounter "settings" ]
    ]
 where
  link' = link router RoutingAction
  tabHeader route label =
    li
      (if state.activeTab == route
        then [className "active"]
        else [])
      [ link'
          route
          []
          [ text label ]
      ]
  tabBody action counter label =
    a [ href "#" , onClick $ const action]
      [ text (label <> " action"), br [] [], text <<< show $ counter ]

update :: forall eff. (Router Route) -> Update State Action (dom :: DOM | eff)
update _ ProfileAction state =
  noEffects $ state { profileCounter = state.profileCounter + 1 }
update _ InboxAction state =
  noEffects $ state { inboxCounter = state.inboxCounter + 1 }
update _ SettingsAction state =
  noEffects $ state { settingsCounter = state.settingsCounter + 1 }
update _ (RoutingAction (Routed r)) state =
  noEffects $ state { activeTab = r }
update _ (RoutingAction (RoutingError _)) state = noEffects state
-- pass other routing actions to router
update router (RoutingAction a) state =
  let r = Pux.Routing.Bob.update router a state
  in  r { effects = (RoutingAction <$> _) <$> r.effects }

main :: forall e. Eff (channel :: CHANNEL, console :: CONSOLE, dom :: DOM, err :: EXCEPTION | e) Unit
main = do
  sampleUrlSignal <- sampleUrl' RoutingAction
  let r = router (Proxy :: Proxy Route)
  case r of
    Just r' -> do
      let
        config =
          { update: update r'
          , view: view r'
          , inputs: [ sampleUrlSignal ]
          , initialState:
              { activeTab: Profile
              , profileCounter: 0
              , inboxCounter: 0
              , settingsCounter: 0
              }
          }
      app <- start config
      renderToDOM "#app" app.html
    Nothing -> pure unit
