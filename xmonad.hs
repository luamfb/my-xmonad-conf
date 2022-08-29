import XMonad
import XMonad.ManageHook
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as W

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Groups as G
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.WindowNavigation

import Data.Monoid

modm = mod1Mask
webBrowser = "firefox"
termEmulator = "alacritty"
menuLauncher = "j4-dmenu-desktop"
fileManager = "pcmanfm"
shutdownCmd = "systemctl poweroff"

volStep = 2
volCmdBase = "pactl set-sink-volume @DEFAULT_SINK@ " 
volUpCmd = volCmdBase ++ "+" ++ (show volStep) ++ "%"
volDownCmd = volCmdBase ++ "-" ++ (show volStep) ++ "%"

brightStep = 1
brightUpCmd = "xbacklight -inc " ++ (show brightStep)
brightDownCmd = "[ `xbacklight -get` -gt 1 ] && xbacklight -dec " ++ (show brightStep)

myTiling = smartBorders $ Tall 1 (3/100) (1/2)
tileGroup = G.group myTiling Full
myFull = windowNavigation $ noBorders Full

groupWsName = "main"
--
-- Purposefully leave 'avoidStruts' out.
-- I do want pannels to be shown only at empty workspaces.
--
myLayoutHook = onWorkspace groupWsName tileGroup $ myFull

myWorkspaces :: [String]
myWorkspaces = ["web", groupWsName, "media", "etc"]

curWsIsGroupWs :: X Bool
curWsIsGroupWs = withWindowSet (\set -> pure ((W.currentTag set) == groupWsName))

focusAbove :: X ()
focusAbove = curWsIsGroupWs >>= (\isGroupWs ->
    if isGroupWs then
        sendMessage $ G.Modify $ G.focusGroupDown
    else
        windows W.focusUp)

focusBelow :: X ()
focusBelow = curWsIsGroupWs >>= (\isGroupWs ->
    if isGroupWs then
        sendMessage $ G.Modify $ G.focusGroupUp
    else
        windows W.focusDown)

moveAbove :: X ()
moveAbove = sendMessage $ G.Modify $ G.moveToGroupUp False

moveBelow :: X ()
moveBelow = sendMessage $ G.Modify $ G.moveToGroupDown False

keybindings = [
    -- window shortcuts
    ((modm, xK_q), kill),

    -- volume and brightness
    ((modm, xK_F2), spawn volDownCmd),
    ((modm, xK_F3), spawn volUpCmd),
    ((modm, xK_F5), spawn brightDownCmd),
    ((modm, xK_F6), spawn brightUpCmd),

    -- cycle windows in StackSet, disregarding layout.
    ((modm, xK_Tab),                windows W.focusUp),
    ((modm .|. shiftMask, xK_Tab),  windows W.focusDown),

    -- navigate / move windows horizontally (group layout only)
    ((modm, xK_h),                  sendMessage $ G.Modify $ G.focusMaster),
    ((modm, xK_l),                  sendMessage $ G.Modify $ G.focusUp),
    ((modm .|. shiftMask, xK_h),    sendMessage $ G.Modify $ G.swapMaster),
    ((modm .|. shiftMask, xK_l),    sendMessage $ G.Modify $ G.swapUp),

    -- navigate windows vertically (in group layout) or in StackSet order
    ((modm, xK_k),                  focusAbove),
    ((modm, xK_j),                  focusBelow),

    -- move windows vertically (group layout only)
    ((modm .|. shiftMask, xK_k),    moveAbove),
    ((modm .|. shiftMask, xK_j),    moveBelow),

    -- launchers
    ((modm, xK_r),        spawn menuLauncher),
    ((modm, xK_e),        spawn fileManager),
    ((modm, xK_w),        spawn webBrowser),
    ((modm, xK_Return),   spawn termEmulator),

    -- last but not least...
    ((mod1Mask .|. controlMask, xK_Delete), spawn shutdownCmd)
    ]

main = xmonad $
    ewmh $
    docks $
    def {
        borderWidth = 1,
        focusedBorderColor = "#2020c0",
        normalBorderColor = "#202020",
        terminal = termEmulator,
        workspaces = myWorkspaces,
        layoutHook = myLayoutHook,
        --manageHook = myManageHook <+> manageHook def,
        focusFollowsMouse = False,
        modMask = modm
        }
    `additionalKeys` keybindings
