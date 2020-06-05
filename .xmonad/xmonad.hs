{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Map as M
import           System.Exit
import           XMonad
import           XMonad.Actions.DynamicWorkspaces
import qualified XMonad.Actions.FlexibleResize as Flex
import           XMonad.Actions.Minimize
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.UpdatePointer
import           XMonad.Hooks.FloatNext
import           XMonad.Hooks.PositionStoreHooks
import           XMonad.Layout.BinarySpacePartition
import qualified XMonad.Layout.BoringWindows as BW
import           XMonad.Layout.ComboP
import           XMonad.Layout.Minimize
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import           XMonad.Prompt
import           XMonad.Prompt.Input
import           XMonad.Prompt.Man
import           XMonad.Prompt.Shell
import qualified XMonad.StackSet as W
import           XMonad.Util.Dmenu
import           XMonad.Util.EZConfig
import qualified XMonad.Util.ExtensibleState as XS
import           XMonad.Util.Minimize
import           XMonad.Util.Run
import           XMonad.Util.Themes

promptConf = def { position = Top }

searchPrompt = do
  s <- inputPrompt conf "Search"
  whenJust s $ \inp -> safeSpawn "firefox" ["--search", inp]
  where conf = def { position = CenteredAt (1/2) (1/2) }
               
myK c =
  mkKeymap c $
  [ ("M-<Return>", spawn $ terminal c)
  , ("M-<Space>", spawn "rofi -modi drun,window -show drun -show-icons")
  , ("M-S-q", kill)
  , ("M-C-<Esc>", io exitSuccess)
  , ("M-S-r", spawn "xmonad --recompile && xmonad --restart")
  
  , ("M-z", windows W.shiftMaster)
  , ("M-x", BW.focusMaster)
  , ("M-<Tab>", BW.focusDown)
  , ("M-S-<Tab>", BW.focusUp)
  
  , ("M-o m", manPrompt promptConf)
  , ("M-o t", shellPrompt promptConf)
  , ("M-o s", searchPrompt)
  
  , ("M-y", toggleScreenSpacingEnabled >> toggleWindowSpacingEnabled)
  , ("M-f", switchLayer)
  , ("M-S-f", toggleFloat)
  , ("M-C-f", toggleFloatNext)

  , ("M-S--", withFocused minimizeWindow)
  , ("M--", withLastMinimized maximizeWindowAndFocus)
  , ("M-C--", showMinimized)
  
  , ("M-.", sendMessage $ IncMasterN 1)
  , ("M-,", sendMessage $ IncMasterN (-1))
  , ("M-S-.", sendMessage NextLayout)
  , ("M-S-,", sendMessage FirstLayout)
  , ("M-e r", sendMessage Rotate)
  , ("M-e e", sendMessage Equalize)
  , ("M-e b", sendMessage Balance)
  , ("M-e p", sendMessage PartitionWins)
  , ("M-s", sendMessage SwapWindow)

  , ("M-w", selectWorkspace promptConf)

  , ("M-p", spawn "playerctl -a pause")
  , ("M-S-p", spawn "playerctl play")
  
  , ("<Print>", spawn "flameshot gui")
  , ("<XF86AudioPlay>", spawn "playerctl play-pause")
  , ("<XF86AudioPrev>", spawn "playerctl previous")
  , ("<XF86AudioNext>", spawn "playerctl next") ]
  ++
  dirKeys
    [ ("k",   "h",   "j",   "l")
    , ("<U>", "<L>", "<D>", "<R>") ]
    
    [ ("M-",      flip windowGo False)
    , ("M-S-",    flip windowSwap False)
    , ("M-C-",    flip screenSwap False)
    , ("M-C-S-",  flip windowToScreen False)
    , ("M-M1-",   resizeLayout)
    , ("M-M1-C-", sendMessage . ShrinkFrom)
    , ("M-M1-S-", sendMessage . MoveSplit) ]
    
    where
      resizeLayout d = do
        l <- getLayout
        case l of
          "BSP" -> sendMessage $ ExpandTowards d
          _ -> case d of
            L -> sendMessage Shrink
            R -> sendMessage Expand
            U -> sendMessage $ IncMasterN 1
            D -> sendMessage $ IncMasterN (-1)
    
dirKeys :: [(String, String, String, String)] -- list of direction keys
        -> [(String, Direction2D -> X ())] -- list of actions for each modifier
        -> [(String, X ())] 
dirKeys dirs actions =
  [(m ++ k, f d) | (m, f) <- actions, (k, d) <- dirkeys]
  where
    dirkeys = concatMap (\(u, l, d, r) -> [(u, U), (l, L), (d, D), (r, R)]) dirs

getLayout :: X String
getLayout = gets $ description . W.layout . W.workspace . W.current . windowset

toggleFloat :: X ()
toggleFloat = withFocused $ \w -> do
  fl <- gets (W.floating . windowset)
  if M.member w fl
    then windows (W.sink w)
    else float w

showMinimized :: X ()
showMinimized = do
  minimized <- XS.gets minimizedStack
  titles <- mapM (\w -> (,w) <$> runQuery title w) minimized
  choice <- dmenuCmd $ M.fromList titles
  whenJust choice maximizeWindowAndFocus
  where dmenuCmd = menuMapArgs "dmenu" ["-i", "-l", "5"]

myMouse XConfig {XMonad.modMask = mod} =
  M.fromList
    [ ((mod, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    , ((mod, button2), windows . (W.shiftMaster .) . W.focusWindow)
    , ((mod, button3), \w -> focus w >> Flex.mouseResizeWindow w >> windows W.shiftMaster) ]

myLayout = tiled ||| tab ||| noBorders Full
  where
    space = spacingRaw
      True -- smart border
      (Border 4 4 4 4) -- screen border
      True -- screen border enabled
      (Border 4 4 4 4) -- window border
      True -- window border enabled
      
    tab = withBorder 0 $ tabbed shrinkText (theme darkTheme)
    
    tall = Tall 1 (1 / 10) (1 / 2)
    
    tiled = minimize
          . BW.boringWindows
          . renamed [CutWordsLeft 1]
          . space
          $ tall ||| emptyBSP ||| Mirror tall

myStartupHook = spawn "~/.fehbg"

myLogHook = updatePointer (0.5, 0.5) (0, 0)

myManageHook = composeAll
  [ positionStoreManageHook Nothing
  , floatNextHook
  , className =? "chatterino" <||> className =? "chatty-Chatty" --> doFloat
  ]

myHandleEventHook = positionStoreEventHook

myconfig = withNavigation2DConfig myNavConfig def
    { terminal = "alacritty"
    , focusedBorderColor = "#00AAFF"
    , normalBorderColor = "#444444"
    , borderWidth = 1
    , modMask = mod4Mask
    , keys = myK
    , layoutHook = myLayout
    , startupHook = myStartupHook
    , handleEventHook = myHandleEventHook
    , logHook = myLogHook
    , mouseBindings = myMouse
    , manageHook = manageHook def <+> myManageHook
    }

myNavConfig = def {defaultTiledNavigation = sideNavigation}

main :: IO ()
main = xmonad myconfig
