{-# LANGUAGE FlexibleContexts #-}
module XMonad.Config.Kiwi.Layout (
  kiwiLayoutHook
) where
import XMonad
import XMonad.Config.Kiwi.Workspaces
       (Workspace, workspaceId, workspaceType, kiwiWorkspaceLookup, kiwiWorkspaceXName)
import XMonad.Config.Kiwi.Utils ((<$))
import XMonad.Hooks.ManageDocks
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.WindowNavigation

-- Define layouts
forceWindow :: (LayoutClass l Window) => l Window -> l Window
forceWindow = id

defaultLayout = Full

kiwiLayoutHook = forceWindow workspacesLayoutHook

-- Workspace layouts
kiwiWorkspaceLayout :: (LayoutClass l1 a, LayoutClass l2 a) => (Workspace -> Bool) -> l1 a -> l2 a -> PerWorkspace l1 l2 a
kiwiWorkspaceLayout p =
  onWorkspaces names
  where
    ws = kiwiWorkspaceLookup p
    names = map (\((x, y), _) -> kiwiWorkspaceXName x y) ws

onKiwiWorkspace :: (LayoutClass l1 a, LayoutClass l2 a) => (Workspace -> Bool) -> l1 a -> l2 a -> PerWorkspace l1 l2 a
onKiwiWorkspace = kiwiWorkspaceLayout

onKiwiWorkspaceId :: (LayoutClass l1 a, LayoutClass l2 a) => String -> l1 a -> l2 a -> PerWorkspace l1 l2 a
onKiwiWorkspaceId wid = onKiwiWorkspace ((==) wid . workspaceId)

onKiwiWorkspaceType :: (LayoutClass l1 a, LayoutClass l2 a) => Maybe String -> l1 a -> l2 a -> PerWorkspace l1 l2 a
onKiwiWorkspaceType wtype = onKiwiWorkspace ((==) wtype . workspaceType)

onKiwiWorkspaces :: (LayoutClass l1 a, LayoutClass l2 a) => [Workspace -> Bool] -> l1 a -> l2 a -> PerWorkspace l1 l2 a
onKiwiWorkspaces preds =
  onKiwiWorkspace (\w -> any (\p -> p w) preds)

onKiwiWorkspaceIds :: (LayoutClass l1 a, LayoutClass l2 a) => [String] -> l1 a -> l2 a -> PerWorkspace l1 l2 a
onKiwiWorkspaceIds workspaceIds =
  onKiwiWorkspaces (map (\wid -> (==) wid . workspaceId) workspaceIds)

onKiwiWorkspaceTypes :: (LayoutClass l1 a, LayoutClass l2 a) => [Maybe String] -> l1 a -> l2 a -> PerWorkspace l1 l2 a
onKiwiWorkspaceTypes workspaceTypes =
  onKiwiWorkspaces (map (\wtype -> (==) wtype . workspaceType) workspaceTypes)

tallLayout = smartSpacing 2 $ windowNavigation $ Tall 1 (3/100) (1/2)

devLayout = (renamed [Replace "Default"] defaultLayout ||| renamed [Replace "Tall"] tallLayout)
  <$ renamed [PrependWords "Dev"]
mediaLayout = smartBorders Full
  <$ renamed [Replace "Media"]

workspacesLayoutHook = defaultLayout
  <$ onKiwiWorkspaceTypes [Just "dev"] devLayout
  <$ onKiwiWorkspaceTypes [Just "media"] mediaLayout
