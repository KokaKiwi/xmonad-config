module XMonad.Config.Kiwi.Config (
  kiwiXConfig,
  kiwiXPConfig,
  kiwiGSConfig,

  kiwiScratchpads
) where
import Text.Printf
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
scratchpadCenterRect :: Rational -> Rational -> W.RationalRect
scratchpadCenterRect w h = W.RationalRect ((1-w)/2) ((1-h)/2) w h

scratchpadCenterFloating :: Rational -> Rational -> ManageHook
scratchpadCenterFloating w h = NSP.customFloating (scratchpadCenterRect w h)

scratchpadsRect :: W.RationalRect
scratchpadsRect = scratchpadCenterRect (2/3) (2/3)

scratchpadsFloating :: ManageHook
scratchpadsFloating = NSP.customFloating scratchpadsRect

kiwiScratchpads :: NSP.NamedScratchpads
kiwiScratchpads =
  [ NSP.NS "htop"       (urxvt "htop" [])               (title =? "htop")           scratchpadsFloating
  , NSP.NS "alsamixer"  (urxvt "alsamixer" [])          (title =? "alsamixer")      scratchpadsFloating
  , NSP.NS "ncmpcpp"    (urxvt "ncmpcpp" [])            (appName =? "ncmpcpp")      (scratchpadCenterFloating (3/5) (3/5))
  , NSP.NS "zim"        "zim"                           (appName =? "zim")          scratchpadsFloating ]
  where
    urxvt :: String -> [String] -> String
    urxvt cmd args = printf "urxvt -name %s -e \"%s\"" cmd (unwords (cmd:args))
