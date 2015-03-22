import XMonad
import XMonad.Config.Kiwi       (kiwiConfig)
import XMonad.Hooks.DynamicLog  (xmobar)

main :: IO ()
main = xmonad =<< xmobar kiwiConfig
