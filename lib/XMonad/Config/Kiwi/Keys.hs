module XMonad.Config.Kiwi.Keys (
  configureKeys
) where
import Data.List
import qualified Data.Map as M
import Text.Printf
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Actions.NoBorders
import XMonad.Actions.Plane
import XMonad.Actions.WindowMenu
import XMonad.Config.Kiwi.Config      (kiwiGSConfig, kiwiXPConfig, kiwiScratchpads)
import XMonad.Config.Kiwi.Workspaces  (kiwiWorkspaceXName, kiwiWorkspaceLookup, workspaceId, kiwiWorkspaceXNames)
import XMonad.Config.Kiwi.Utils       ((<$), showMessage)
import XMonad.Config.Kiwi.Utils.Keys  (addKeyBindings)
import XMonad.Layout.WindowNavigation
import XMonad.Prompt.XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad

configureKeys :: XConfig l -> XConfig l
configureKeys conf = conf
  `addKeyBindings` customKeys
  `addKeyBindings` scratchpadsKeys
  `addKeyBindings` gridKeys
  `addKeyBindings` windowNavigationKeys
  `addKeyBindings` workspacesPlaneKeys

-- Window navigation
windowNavigationKeys :: XConfig Layout -> [((ButtonMask, KeySym), X ())]
windowNavigationKeys conf =
  [ ((modMask conf,               xK_Right),  sendMessage $ Go R)
  , ((modMask conf,               xK_Left),   sendMessage $ Go L)
  , ((modMask conf,               xK_Up),     sendMessage $ Go U)
  , ((modMask conf,               xK_Down),   sendMessage $ Go D)

  , ((modMask conf .|. shiftMask, xK_Right),  sendMessage $ Swap R)
  , ((modMask conf .|. shiftMask, xK_Left),   sendMessage $ Swap L)
  , ((modMask conf .|. shiftMask, xK_Up),     sendMessage $ Swap U)
  , ((modMask conf .|. shiftMask, xK_Down),   sendMessage $ Swap D) ]

-- Grid Select actions
gridKeys :: [(String, X ())]
gridKeys =
  [ ("M-f",     runSelectedAction kiwiGSConfig favorites)
  , ("M-S-s",   runSelectedAction kiwiGSConfig screens)
  , ("M-S-d",   runSelectedAction kiwiGSConfig debug) ]
  where
    screens =
      [ (name, spawn ("~/.screenlayout/" ++ filename ++ ".sh"))
      | (name, filename) <- screenNames ] ++
      [ ("Arandr", spawn "arandr") ]

screenNames :: [(String, String)]
screenNames =
  [ ("Simple", "simple")
  , ("Dual",   "double") ]

favorites :: [(String, X ())]
favorites =
  [ ("Chromium",        spawn "chromium")
  , ("Sublime Text 3",  spawn "~/sublime_text_3/sublime_text")
  , ("Lock screen",     spawn "~/scripts/lock_screen.sh") ]

debug :: [(String, X ())]
debug =
  [ ("Workspaces",      showMessage ("Workspaces: " ++ unwords workspaces'' ++ "\nKiwi workspaces: " ++ unwords workspaces')) ]
  where
    ids = ["epitech", "dev.1", "dev.2", "dev.3"]
    ws = kiwiWorkspaceLookup (\w -> workspaceId w `elem` ids)
    workspaces' :: [String]
    workspaces' = zipWith (printf "%s/%s") workspaceNames workspaceIds
    workspaces'' = kiwiWorkspaceXNames
    workspaceNames = map (\((x, y), _) -> kiwiWorkspaceXName x y) ws
    workspaceIds = map (\(_, w) -> workspaceId w) ws

-- Workspaces plane actions
workspacesPlaneKeys :: XConfig Layout -> [((KeyMask, KeySym), X ())]
workspacesPlaneKeys conf = M.assocs $
  planeKeys (modMask conf .|. controlMask) (Lines 3) Circular

-- Scratchpads keys
scratchpadsKeys :: XConfig Layout -> [((ButtonMask, KeySym), X ())]
scratchpadsKeys conf =
  [ ((noModMask,      xK_twosuperior),    namedScratchpadAction kiwiScratchpads "htop")
  , ((modMask conf,   xK_twosuperior),    namedScratchpadAction kiwiScratchpads "alsamixer") ]

-- Custom
customKeys :: XConfig Layout -> [((ButtonMask, KeySym), X ())]
customKeys conf =
  [ ((modMask conf,   xK_l),              spawn "~/scripts/lock_screen.sh")
  , ((modMask conf,   xK_s),              spawn "~/sublime_text_3/sublime_text")
  , ((modMask conf,   xK_w),              spawn "~/scripts/wallpaper_cron.sh")

  , ((modMask conf,   xK_o),              windowMenu)
  , ((modMask conf,   xK_F2),             xmonadPrompt kiwiXPConfig)
  , ((modMask conf,   xK_g),              withFocused toggleBorder)
  , ((modMask conf .|. controlMask, xK_w), gridselectWorkspace kiwiGSConfig W.greedyView) ]
