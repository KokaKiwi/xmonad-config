module XMonad.Config.Kiwi.Config (
  kiwiXConfig,
  kiwiXPConfig,
  kiwiGSConfig,

  kiwiScratchpads
) where
import XMonad
import XMonad.Actions.GridSelect
       (HasColorizer, GSConfig, defaultGSConfig)
import XMonad.Config.Azerty (azertyConfig)
import XMonad.Prompt (XPConfig, defaultXPConfig)
import XMonad.StackSet as W
import qualified XMonad.Util.NamedScratchpad as NSP

-- TODO: See http://xmonad.org/xmonad-docs/xmonad-contrib/src/XMonad-Actions-WindowMenu.html

-- XMonad main config
kiwiXConfig = azertyConfig
  { terminal                = "urxvt"
  , modMask                 = mod4Mask  -- Win key
  , normalBorderColor       = "#cccccc"
  , focusedBorderColor      = "#cd8b00" }

-- XMonad.Prompt config
kiwiXPConfig :: XPConfig
kiwiXPConfig = defaultXPConfig

-- XMonad.Actions.GridSelect config
kiwiGSConfig :: (HasColorizer a) => GSConfig a
kiwiGSConfig = defaultGSConfig

-- XMonad.Util.NamedScratchpad pads
scratchpadsRect :: W.RationalRect
scratchpadsRect = W.RationalRect (1/6) (1/6) (2/3) (2/3)

scratchpadsFloating :: ManageHook
scratchpadsFloating = NSP.customFloating scratchpadsRect

kiwiScratchpads :: NSP.NamedScratchpads
kiwiScratchpads =
  [ NSP.NS "htop"       "urxvt -e \"htop\""       (title =? "htop")       scratchpadsFloating
  , NSP.NS "alsamixer"  "urxvt -e \"alsamixer\""  (title =? "alsamixer")  scratchpadsFloating ]
