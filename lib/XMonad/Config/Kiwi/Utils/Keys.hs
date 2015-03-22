{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module XMonad.Config.Kiwi.Utils.Keys (
  KeyBindings(..),
  addKeyBindings
) where
import qualified Data.Map as M
import XMonad
import XMonad.Util.EZConfig (mkKeymap)

-- Types
type XKeyMap = M.Map (ButtonMask, KeySym) (X ())
type XConfigKeys = XConfig Layout -> XKeyMap

-- Classes
class KeyBindings b where
  getKeys :: b -> XConfigKeys

  getKeyMap :: (KeyBindings b) => XConfig Layout -> b -> XKeyMap
  getKeyMap config bindings = getKeys bindings config

  mergeKeys :: (KeyBindings b) => XConfigKeys -> b -> XConfigKeys
  mergeKeys keys bindings config =
    M.union (getKeyMap config bindings) (keys config)

  mergeConfig :: (KeyBindings b) => XConfig l -> b -> XConfig l
  mergeConfig config bindings =
    config { keys = mergeKeys (keys config) bindings }

instance KeyBindings (M.Map (ButtonMask, KeySym) (X ())) where
  getKeys bindings _ = bindings

instance KeyBindings [((ButtonMask, KeySym), X ())] where
  getKeys bindings = getKeys $ M.fromList bindings

instance KeyBindings [(String, X ())] where
  getKeys bindings config = mkKeymap config bindings

instance (KeyBindings b) => KeyBindings (XConfig Layout -> b) where
  getKeys f config = getKeyMap config (f config)

addKeyBindings :: (KeyBindings b) => XConfig l -> b -> XConfig l
addKeyBindings = mergeConfig
