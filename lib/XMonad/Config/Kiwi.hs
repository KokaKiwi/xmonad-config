module XMonad.Config.Kiwi (
  kiwiConfig
) where
import XMonad
import XMonad.Config.Kiwi.Config      (kiwiXConfig)
import XMonad.Config.Kiwi.Keys        (configureKeys)
import XMonad.Config.Kiwi.Layout      (kiwiLayoutHook)
import XMonad.Config.Kiwi.ManageHook  (kiwiManageHook)
import XMonad.Config.Kiwi.Utils       ((<$))
import XMonad.Config.Kiwi.Workspaces  (kiwiWorkspaceXNames)
import XMonad.Hooks.ICCCMFocus        (takeTopFocus)

kiwiConfig = kiwiXConfig
  { workspaces        = kiwiWorkspaceXNames
  , layoutHook        = kiwiLayoutHook
  , manageHook        = kiwiManageHook kiwiXConfig
  , logHook           = takeTopFocus }
  <$ configureKeys
