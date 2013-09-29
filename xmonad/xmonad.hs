import XMonad

import Data.Monoid
import Data.Ratio
import System.Exit
import Control.Exception
import System.IO
import System.Directory

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import System.IO
import XMonad.Util.Loggers
import XMonad.Util.Run
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Spiral
import XMonad.Layout.SimpleFloat
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Hooks.EwmhDesktops

import XMonad.Prompt
import XMonad.Prompt.Window

myTerminal      = "urxvtc"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse =True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth   = 1

myModMask       = mod1Mask

myWorkspaces    = clickable. (map dzenEscape) $ ["1","2","3","4","5"]    
  where clickable l     = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                            (i,ws) <- zip [1..] l,
                            let n = i ]
 

myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    , ((modm,               xK_p     ), spawn "dmenu_run")
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    , ((modm .|. shiftMask, xK_c     ), kill)

    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    , ((modm,               xK_n     ), refresh)

    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm,               xK_m     ), windows W.focusMaster  )

    , ((modm,               xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)

    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    
    , ((modm              , xK_b     ), sendMessage $ ToggleStrut D)
    , ((modm .|. shiftMask, xK_b     ), sendMessage ToggleStruts)

    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    , ((modm .|. shiftMask, xK_l     ), spawn "xscreensaver-command -lock | logger")

    , ((modm              , xK_a     ), windowPromptGoto defaultXPConfig { autoComplete = Just 500000 })
    ]
    ++

    

    
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [ ((modm .|. controlMask,   xK_Right     ), nextWS)
    , ((modm .|. shiftMask,     xK_Right     ), shiftToNext)
    , ((modm .|. controlMask,   xK_Left      ), prevWS)
    , ((modm .|. shiftMask,     xK_Left      ), shiftToPrev)]
    ++
    

    
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    ]

myLayout = avoidStrutsOn [U,L,R] $ smartBorders $ onWorkspace "3" custom3 $ onWorkspace "2" custom2 $ custom1

myTiled   = Tall nmaster delta ratio
  where

     nmaster = 1

     ratio   = 1/2

     delta   = 3/100

custom1 = myTiled ||| Mirror myTiled ||| Full ||| spiral (1 % 1)
custom2 = Mirror myTiled ||| myTiled ||| Full
custom3 = simpleFloat ||| Full ||| Mirror myTiled ||| myTiled

myManageHook =  manageDocks <+> composeAll ( concat (
    [ [ className =? c        --> doShift "2" | c <- myWebs]
    , [ className =? c        --> doIgnore    | c <- myIgnore]
    , [ resource  =? "Dialog" --> doFloat]
    , [ className =? f        --> doFloat | f <- myFloats]
    ] ))

myWebs = ["Chromium","Midori"]
myFloats = ["Angband", "DeaDBeeF", "Aumix", "Orage", "Xfce4-appfinder"]
myIgnore = ["Conky"]

myEventHook = ewmhDesktopsEventHook

myDzenPP  = dzenPP
    { ppCurrent = dzenColor "#3399ff" "" . wrap " " " "
    , ppHidden  = dzenColor "#dddddd" "" . wrap " " " "
    , ppHiddenNoWindows = dzenColor "#777777" "" . wrap " " " "
    , ppUrgent  = dzenColor "#ff0000" "" . wrap " " " "
    , ppSep     = " | "
    , ppLayout  = dzenColor "#aaaaaa" "" . wrap "^ca(1,xdotool key super+space)· " " ·^ca()"
    , ppTitle   = dzenColor "#ffffff" "" 
                    . wrap "^ca(1,xdotool key super+k)^ca(2,xdotool key super+shift+c)"
                           "^ca()^ca()" . pad . fixToWidth 9 . dzenEscape
    , ppOrder = reverse
    }
  where
  fixToWidth :: Int -> String -> String
  fixToWidth a s = let ls = length s in if ls < a then s ++ (replicate (a-ls) ' ') else shorten a s 
 
myLogHook myDzen = dynamicLogWithPP $ myDzenPP { 
  ppOutput = hPutStrLn myDzen,
  ppExtras = [wrapL "bat: " "" battery, 
              wrapL "^ca(1, aumix)" "^ca()" $ shortenL 10 $ aumixVolume, 
              wrapL "^ca(1, orage)" "^ca()" $ date "%b %d %H:%M"] }

myStartupHook = ewmhDesktopsStartup >> do
  spawn $ "xscreensaver -no-splash"
  spawn $ "urxvtd"
  spawn $ "conky -qdc " ++ myConfigDir ++ "conkyMessages.conf"
  spawn $ "conky -qdc " ++ myConfigDir ++ "conkyStats.conf"
  spawn $ "sleep 1 && " ++ myTerminal

data MyConfig = MyConfig { border :: Int }
getConfig :: IO MyConfig
getConfig = do
  l <- doesFileExist config 
  if not l then defaultConf else do
    str <- readFile config  
    let l = lines str in case reads (l !! 0) of 
      [(x,"")] -> return $ MyConfig x 
      _      -> defaultConf
  where 
  defaultConf = return $ MyConfig defaultBor
  config = ".xmonad/" ++ myConfigDir ++" xmonad.conf"
  defaultBor = 650

myXmonadBar c = "dzen2 -y '0' -h '24'  -w '" ++ show (border c) ++ "' -ta 'l'" ++ myDzenStyle
myStatusBar c = "conky -c " ++ myConfigDir ++ "/conkyDzen.conf | dzen2 -x '" ++ show (border c) ++ "'  -h '24' -ta 'r' -y '0'" ++ myDzenStyle
myConfigDir = "$HOME/.xmonad/confs/"
myBitmapsDir = "$HOME/.xmonad/dzen2"
myDzenStyle  = " -h '20' -fg '#777777' -bg '#222222' -fn 'monospace:size=10'"

main = do
  myConfig <- getConfig
  myDzen <- spawnPipe $ myXmonadBar myConfig
  dzenRightBar <- spawnPipe $ myStatusBar myConfig
  xmonad defaultConfig {

        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

        keys               = myKeys,
        mouseBindings      = myMouseBindings,

        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook myDzen >> fadeInactiveLogHook 0xdddddddd,
        startupHook        = myStartupHook
    }
