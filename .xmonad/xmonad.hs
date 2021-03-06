
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Combo
import XMonad.Layout.Decoration
import XMonad.Layout.Maximize
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.StackSet
import XMonad.Util.Loggers
import XMonad.Util.Run
import XMonad.Util.EZConfig
import Control.Arrow ((&&&),first)
import Data.List (partition)

import System.IO
import System.Exit

import qualified XMonad.StackSet as W hiding (swapDown, swapUp)
import qualified Data.Map        as M



  ------------------------------------------------------------------------
-- Simple options.
--
myModMask = mod5Mask
myTerminal = "urxvtc"
myFocusFollowsMouse = False
myBorderWidth = 0
myWorkspaces = ["1:term", "2:web"  , "3:code"
               ,"4:serv", "5:comm" , "6:gimp"
               ,"7"     , "8:music", "9:float"]

------------------------------------------------------------------------
-- Custom functions go here.
--

swapUp'  (W.Stack t (l:ls) rs) = W.Stack t ls (l:rs)
swapUp'  (W.Stack t []     rs) = W.Stack t (rot $ reverse rs) []
    where rot (x:xs) = xs ++ [x]
          rot _ = []

swapUp = W.modify' swapUp'

reverseStack (W.Stack t ls rs) = W.Stack t rs ls

swapDown = W.modify' (reverseStack . swapUp' . reverseStack)

taggedStacks :: [W.Workspace i l a] -> [(i, [a])]
taggedStacks = map $ W.tag &&& W.integrate' . W.stack

partCurrent :: (Eq a, Eq i) => W.StackSet i l a s sd -> [(i, [a])] -> ([a], [(i, [a])])
partCurrent ws = first (snd . head) . partition ((W.currentTag ws ==) . fst)

hasCopies :: (Eq a) => ([a], [(i, [a])]) -> [i]
hasCopies (curs, oths) = map fst $ Prelude.filter (any (`elem` curs) . snd) $ oths

wsContainingCopiesM :: X [WorkspaceId]
wsContainingCopiesM = withWindowSet $ \ws ->return . hasCopies . partCurrent ws . taggedStacks $ W.workspaces ws

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

-- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
      -- launch chromium
    , ((modm, xK_Return), spawn "chromium-bin")
      -- launch alpine in urxvt
    , ((modm,               xK_a     ), spawn "urxvt -e alpine")

      -- launch shell prompt menu
    , ((modm,               xK_r     ), shellPrompt myXPConfig)

      -- runOrRaisePrompt
    , ((modm .|. shiftMask, xK_r     ), runOrRaisePrompt myXPConfig)

      -- close focused window
    , ((modm .|. shiftMask, xK_k     ), kill)

      -- close coppied window
    , ((modm,               xK_k     ), kill1)

      -- Rotate forward through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

      -- TODO: add PrevLayout
      -- Rotate backward through the available layout algorithms
      --, ((modm .|. controlMask, xK_space ), sendMessage PrevLayout)

      --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

      -- Resize viewed windows to the correct size
    , ((modm .|. controlMask, xK_r   ), refresh)

      -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

      -- Move focus to the next window
    , ((modm,               xK_e     ), windows W.focusDown)

      -- Move focus to the previous window
    , ((modm,               xK_i     ), windows W.focusUp  )

      -- Move focus to the master window
    , ((modm,               xK_h     ), windows W.focusMaster  )

      -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

      -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_e     ), windows Main.swapDown  )

      -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_i     ), windows Main.swapUp    )

      -- Shrink the master area
    , ((modm,               xK_n     ), sendMessage Shrink)

      -- Expand the master area
    , ((modm,               xK_o     ), sendMessage Expand)

      -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

      -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

      -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

      -- toggle the status bar gap
      -- TODO, update this binding with avoidStruts
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

      -- To maximize a window
    , ((modm              , xK_m     ), withFocused $ sendMessage . maximizeRestore)

      -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

      -- Restart xmonad
    , ((modm              , xK_q     ), restart "xmonad" True)

    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    -- mod-control-[1..9], Copy client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask), (copy, controlMask)]
    ]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    --
    --[((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    -- ++

    -- To make the combineTwo layout work
    [ ((modm .|. controlMask .|. shiftMask, xK_o), sendMessage $ Move R)
    , ((modm .|. controlMask .|. shiftMask, xK_n), sendMessage $ Move L)
    , ((modm .|. controlMask .|. shiftMask, xK_i), sendMessage $ Move U)
    , ((modm .|. controlMask .|. shiftMask, xK_e), sendMessage $ Move D)
    ]
    ++

    -- For ResizableTall layout
    [ ((modm,                    xK_u), sendMessage MirrorShrink)
    , ((modm,                    xK_y), sendMessage MirrorExpand)
    ]
    ++

    -- screenshot screen
    [ ((modm,               xK_Print     ), spawn "/usr/bin/screenshot scr")

    -- screenshot window or area
    , ((modm .|. shiftMask, xK_Print     ), spawn "/usr/bin/screenshot win")
    ]


------------------------------------------------------------------------
-- Define all the possible layouts here.
--

myLayouts = onWorkspace "6:gimp" gimp
            $ avoidStruts ( tiled' ||| Mirror tiled' ||| myTabbed ||| Full ||| combo ||| simplestFloat ) ||| Full
  where
    -- The default number of windows in the master pane
    nmaster  = 1
    -- Default proportion of screen occupied by master pane
    ratio    = 1/2
    -- Percent of screen to increment by when resizing panes
    delta    = 3/100
    -- Tabbed Layout
    myTabbed = tabbed shrinkText myTabTheme
    -- Shorthand for ResizableTall
    tiled'   = maximize $ ResizableTall nmaster delta ratio []
    -- Combined layout
    combo    = windowNavigation ( combineTwo (TwoPane delta ratio) (myTabbed) (tiled') )
    -- Gimp layout
    gimp = avoidStruts $ withIM (3/20) (Role "gimp-toolbox")
           $ reflectHoriz
           $ withIM (3/20) (Role "gimp-dock") $ myTabbed


------------------------------------------------------------------------
-- Statusbar configuration.
--

statusBarCmd :: String
statusBarCmd = "dzen2" ++
               " -bg '" ++ colorDarkGray ++ "'" ++
               " -fg '" ++ colorLightGray ++ "'" ++
               " -sa l" ++
               " -fn '" ++ barXFont ++ "'" ++
               " -ta l -e ''"

------------------------------------------------------------------------
-- Theme configuration.
--
colorBlack, colorDarkGray, colorLightGray, colorRed, colorCyan, colorWhite :: [Char]
-- colorBlack           = "#000000"
-- colorDarkGray        = "#222222"
-- colorLightGray       = "#aaaaaa"
-- colorLightBlue       = "#0066ff"
-- colorWhite           = "#ffffff"
-- colorRed             = "#ff0000"
-- colorCyan            = "#00ffff"
-- colorMagenta         = "#ff00fd"
-- colorBlue            = "#003cfd"
-- colorGreen           = "#00ff00"
-- colorYellow          = "#fdfd00"
colorBlack           = "#002b36"
colorDarkGray        = "#586e75"
colorLightGray       = "#eee8d5"
colorLightBlue       = "#cb4b16"
colorWhite           = "#fdf6e3"
colorRed             = "#dc322f"
colorCyan            = "#2aa198"
colorMagenta         = "#d33682"
colorBlue            = "#268bd2"
colorGreen           = "#859900"
colorYellow          = "#b58900"

barFont, barXFont, largeBarXFont    :: [Char]
barFont              = "dina"
barXFont             = "-*-dina-medium-r-*-*-16-*-*-*-*-*-*-*"
largeBarXFont = "-*-dina-medium-r-*-*-22-*-*-*-*-*-*-*"

 ------------------------------------------------------------------------
-- My Own PP.
--
myPP :: PP
myPP = dzenPP { ppCurrent  = dzenColor colorWhite colorLightBlue . activeDwmPad
              , ppHidden   = dzenColor colorLightGray colorDarkGray . activeDwmPad
              , ppHiddenNoWindows = const ""
              , ppSep      = "|"
              , ppLayout   = dzenColor colorWhite colorDarkGray .
                             (\ x -> case x of
                                 "Maximize ResizableTall"        -> " []= "
                                 "Mirror Maximize ResizableTall" -> " TTT "
                                 "Tabbed Simplest"               -> " [=] "
                                 "Full"                          -> " [ ] "
                                 "combining Tabbed Simplest and Maximize ResizableTall with TwoPane"
                                   -> " []+ "
                                 "SimplestFloat"                 -> " ><> "
                                 _                               -> pad x
                             )
              , ppTitle    = dzenColor colorWhite colorDarkGray . pad
              }
  where
    activeDwmPad a = "^i(/usr/home/joshua/.xpms/active.xpm)" ++ a ++ " "

------------------------------------------------------------------------
-- My Own logHook
--
myLogHook h = do copies <- wsContainingCopiesM
                 currentWS <- gets $ peek . windowset
                 let inactiveCheck ws | ws `elem` copies = dzenColor colorLightGray colorDarkGray . inactiveDwmCopiesPad $ ws
                                      | otherwise = inactiveDwmPad ws
                 let activeCheck ws | currentWS /= Nothing = dzenColor colorWhite colorLightBlue $ activeDwmPad ws
                                    | otherwise = dzenColor colorWhite colorLightBlue $ pad ws
                 dynamicLogWithPP $ myPP { ppHidden = inactiveCheck, ppCurrent = activeCheck, ppOutput = hPutStrLn h }
    where
      activeDwmPad a = "^i(/usr/home/joshua/.xpms/active.xpm)" ++ a ++ " "
      inactiveDwmPad a = "^i(/usr/home/joshua/.xpms/inactive.xpm)" ++ a ++ " "
      inactiveDwmCopiesPad a = "^i(/usr/home/joshua/.xpms/inactiveCopies.xpm)" ++ a ++ " "






------------------------------------------------------------------------
-- My Own XPConfig
--
myXPConfig :: XPConfig
myXPConfig = defaultXPConfig { font        = largeBarXFont
                             , bgColor     = colorDarkGray
                             , fgColor     = colorLightGray
                             , bgHLight    = colorYellow
                             , fgHLight    = colorWhite
                             , borderColor = colorLightBlue
                             }

------------------------------------------------------------------------
-- My Own Tab Theme
--
myTabTheme :: Theme
myTabTheme = defaultTheme { fontName = barXFont
                          , activeColor = colorBlue
                          , inactiveColor = colorDarkGray
                          , activeBorderColor = colorLightGray
                          , inactiveBorderColor = colorBlue
                          , activeTextColor = colorWhite
                          , inactiveTextColor = colorLightGray
                          }

------------------------------------------------------------------------
-- main function.
--
main = do h <- spawnPipe statusBarCmd -- For the left side of the status bar (dzen)
          spawn "~/bin/dzenscript &" -- and the right side
          xmonad $ defaultConfig { modMask = myModMask
                                 , terminal = myTerminal
                                 , borderWidth = myBorderWidth
                                 , focusFollowsMouse = myFocusFollowsMouse
                                 , XMonad.workspaces = myWorkspaces
                                 , keys = myKeys
                                 , logHook = myLogHook h
                                 , layoutHook = myLayouts
                                 }
            -- audio controls
            `additionalKeysP` [
                              ]
