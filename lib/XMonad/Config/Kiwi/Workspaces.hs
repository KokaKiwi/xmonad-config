module XMonad.Config.Kiwi.Workspaces (
  Workspace(..),

  workspaceNames,
  workspaceXName,
  workspaceXNames,

  kiwiWorkspaces,
  kiwiWorkspacesSize,
  kiwiWorkspacesWidth,
  kiwiWorkspacesHeight,

  kiwiWorkspace,
  kiwiWorkspaceIndex,
  kiwiWorkspaceName,
  kiwiWorkspaceNames,
  kiwiWorkspaceXName,
  kiwiWorkspaceXNames,
  kiwiWorkspaceLookup,
  kiwiWorkspaceLookupName
) where
import Text.Printf
import XMonad
import XMonad.Config.Kiwi.Utils   (enumerate)

-- Workspace operations
data Workspace = Workspace
  { workspaceId         :: String
  , workspaceName       :: String
  , workspaceType       :: Maybe String }

workspaceNames :: [[Workspace]] -> [[String]]
workspaceNames = map $ map workspaceName

workspaceXName :: Int -> Workspace -> String
workspaceXName n w = printf "%d:%s" n (workspaceName w)

workspaceXNames :: [[Workspace]] -> [String]
workspaceXNames ws = map makeName indexedWorkspaces
  where
    makeName (n, w) = workspaceXName (n + 1) w
    indexedWorkspaces = enumerate $ concat ws

-- Define workspaces
mainWorkspace :: Workspace
mainWorkspace = Workspace
  { workspaceId   = "main"
  , workspaceName = "Main"
  , workspaceType = Nothing }

epitechWorkspace :: Workspace
epitechWorkspace = Workspace
  { workspaceId   = "epitech"
  , workspaceName = "Epitech"
  , workspaceType = Just "dev" }

mediaWorkspace :: Workspace
mediaWorkspace = Workspace
  { workspaceId   = "media"
  , workspaceName = "Media"
  , workspaceType = Just "media" }

miscWorkspace :: Workspace
miscWorkspace = Workspace
  { workspaceId   = "misc"
  , workspaceName = "Misc"
  , workspaceType = Nothing }

devWorkspace :: Int -> Workspace
devWorkspace n = Workspace
  { workspaceId   = printf "dev.%d" n
  , workspaceName = printf "Dev [%d]" n
  , workspaceType = Just "dev" }

kiwiWorkspaces :: [[Workspace]]
kiwiWorkspaces =
  [ [mainWorkspace,   epitechWorkspace,   devWorkspace 1]
  , [mediaWorkspace,  miscWorkspace,      devWorkspace 2]
  , [miscWorkspace,   miscWorkspace,      devWorkspace 3] ]

kiwiWorkspacesSize :: (Int, Int)
kiwiWorkspacesSize = (length (head kiwiWorkspaces), length kiwiWorkspaces)

kiwiWorkspacesWidth :: Int
kiwiWorkspacesWidth = width
  where
    (width, _) = kiwiWorkspacesSize

kiwiWorkspacesHeight :: Int
kiwiWorkspacesHeight = height
  where
    (_, height) = kiwiWorkspacesSize

kiwiWorkspace :: Int -> Int -> Workspace
kiwiWorkspace x y = kiwiWorkspaces !! y !! x

kiwiWorkspaceIndex :: Int -> Int -> Int
kiwiWorkspaceIndex x y = y * 3 + x

kiwiWorkspacePos :: Int -> (Int, Int)
kiwiWorkspacePos index = (index `mod` 3, index `div` 3)

kiwiWorkspaceName :: Int -> Int -> String
kiwiWorkspaceName x y = workspaceName $ kiwiWorkspace x y

kiwiWorkspaceNames :: [[String]]
kiwiWorkspaceNames = workspaceNames kiwiWorkspaces

kiwiWorkspaceXName :: Int -> Int -> String
kiwiWorkspaceXName x y = workspaceXName (n + 1) w
  where
    n = kiwiWorkspaceIndex x y
    w = kiwiWorkspace x y

kiwiWorkspaceXNames :: [String]
kiwiWorkspaceXNames = workspaceXNames kiwiWorkspaces

kiwiWorkspaceLookup :: (Workspace -> Bool) -> [((Int, Int), Workspace)]
kiwiWorkspaceLookup p = filter p' ws
  where
    p' (_, w) = p w
    ws = zip (map kiwiWorkspacePos [0..]) (concat kiwiWorkspaces)

kiwiWorkspaceLookupName :: String -> [((Int, Int), Workspace)]
kiwiWorkspaceLookupName name = kiwiWorkspaceLookup $ (==) name . workspaceName
