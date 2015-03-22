module XMonad.Config.Kiwi.Utils (
  (<$),

  enumerate,
  showMessage
) where
import XMonad

-- Reversed apply operator
(<$) :: a -> (a -> b) -> b
a <$ f = f a

-- Util functions
enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

-- Debug functions
showMessage :: String -> X ()
showMessage msg = spawn ("echo -e \"" ++ msg ++ "\" | xmessage -nearmouse -center -file -")
