{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main (main)
  where

import Prelude ((+))

import Control.Applicative ((*>))
import Data.Bits ((.|.))
import Data.Function (($), (.), id)
import Data.Monoid ((<>), mconcat)
import Data.Ord ((<=))
import Data.String (String)
import System.IO (IO)

import Data.Map (Map)
import qualified Data.Map as Map (fromList)
import XMonad
    ( Button
    , KeyMask
    , KeySym
    , Window
    , X
    , XConfig
        ( XConfig
        , keys
        , layoutHook
        , logHook
        , manageHook
        , modMask
        , mouseBindings
        , terminal
        , workspaces
        )
    , button1
    , button2
    , button3
    , mod1Mask
    , mod3Mask
    , mod4Mask
    , shiftMask
    , spawn
    , xK_0
    , xK_Tab
    , xK_l
    , xK_r
    , xmonad
    )
import XMonad.Config.Xfce (xfceConfig)
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.ManageHook ((=?), (-->), appName, title)
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import qualified XMonad.Operations as Operations
    ( focus
    , mouseMoveWindow
    , mouseResizeWindow
    , windows
    )
import qualified XMonad.StackSet as StackSet
    ( focusDown
    , focusUp
    , focusWindow
    , greedyView
    , shift
    , shiftMaster
    )

import qualified Local.XMonad as Util (workspaceKeys)

main :: IO ()
main = xmonad $ go xfceConfig
  where
    go cfg@XConfig{keys, manageHook} = cfg
        { layoutHook = noBorders (layoutHook cfg)
        , logHook = fadeInactiveLogHook 0.5 *> logHook cfg
        , manageHook = manageHook <> mconcat
            [ title =? "Application Finder" --> doCenterFloat
            , appName =? "keepassx" --> doCenterFloat
            ]
        , workspaces = myWorkspaces
        , modMask = mod4Mask
        , keys = myKeys <> keys -- Local definitions have priority.
        , mouseBindings = myMouseBindings
        , terminal = "urxvt"
        }

myWorkspaces :: [String]
myWorkspaces =
    [ "α" -- 0
    , "β" -- 1
    , "γ" -- 2
    , "δ" -- 3
    , "ε" -- 4
    , "ζ" -- 5
    , "η" -- 6
    , "θ" -- 7
    , "ι" -- 8
    , "κ" -- 9
    ]

-- mod-r         -- Run command.
-- shift-mod-l   -- Lock screen.
--
-- mod-[0..9]    -- Switch to workspace N.
-- alt-[0..9]    -- Move client to workspace N.
--
-- alt-tab       -- Focus next window.
-- alt-shift-tab -- Focus previous window.
myKeys :: XConfig a -> Map (KeyMask, KeySym) (X ())
myKeys cfg@XConfig{modMask} = runCommands <> workspaceKeys <> switchWindowFocus
  where
    runCommands = Map.fromList
        [ ((modMask, xK_r), spawn "xfce4-appfinder --collapsed")
        , ((modMask .|. shiftMask, xK_l), spawn "xflock4")
            -- Command xflock4 is a shell script that invokes correct locking
            -- application, if available.
        ]

    workspaceKeys = Util.workspaceKeys cfg $ \_cfg name idx ->
        if idx <= 9
            then
                [ ((m, idx + xK_0), Operations.windows $ f name)
                | (f, m) <-
                    [ (StackSet.greedyView, modMask)
                    , (StackSet.shift,      mod1Mask)   -- Left ALT
                    , (StackSet.shift,      mod3Mask)   -- Right ALT
                    ]
                ]
            else []

    switchWindowFocus = Map.fromList
        [ ((g m, xK_Tab), Operations.windows f)
        | m <- [mod1Mask, mod3Mask]
        , (g, f) <-
            [ (id,              StackSet.focusDown)
            , ((.|. shiftMask), StackSet.focusUp  )
            ]
        ]

-- alt-button1 -- Set the window to floating mode and move by dragging.
-- alt-button2 -- Raise the window to the top of the stack.
-- alt-button3 -- Set the window to floating mode and resize by dragging.
myMouseBindings :: XConfig a -> Map (KeyMask, Button) (Window -> X ())
myMouseBindings _ = Map.fromList $ mconcat
    [ [ ((m, button1), makeFloatingWindowAndDragIt)
      , ((m, button2), raiseWindowToTheTopOfTheStack)
      , ((m, button3), makeFloatingWindowAndResizeIt)
      ]
    | m <- [mod1Mask, mod3Mask]
    ]
  where
    makeFloatingWindowAndDo action w = do
        Operations.focus w
        _ <- action w
        Operations.windows StackSet.shiftMaster

    makeFloatingWindowAndDragIt =
        makeFloatingWindowAndDo Operations.mouseMoveWindow

    makeFloatingWindowAndResizeIt =
        makeFloatingWindowAndDo Operations.mouseResizeWindow

    raiseWindowToTheTopOfTheStack =
        Operations.windows . (StackSet.shiftMaster .) . StackSet.focusWindow
