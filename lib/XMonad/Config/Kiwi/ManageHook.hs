module XMonad.Config.Kiwi.ManageHook (
  kiwiManageHook
) where
import XMonad
import XMonad.Config.Kiwi.Config (kiwiScratchpads)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Place
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad

kiwiManageHook :: XConfig l -> ManageHook
kiwiManageHook conf =
  manageDocks <+>
  scratchpadManageHook <+>
  placeHook simpleSmart <+>
  appRules <+>
  manageHook conf

-- ScratchPad config
scratchpadManageHook :: ManageHook
scratchpadManageHook = namedScratchpadManageHook kiwiScratchpads

-- Application rules
appRules :: ManageHook
appRules = composeAll
  [ className =? "Arandr"       --> doFloat
  , className =? "Instantbird"  --> doFloat
  , className =? "Xmessage"     --> doFloat
  , className =? "Pidgin"       --> doFloat
  , className =? "Gajim"        --> doFloat
  , className =? "Thunar"       --> doFloat ]
