{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main (main)
  where

import Prelude ((+))

import Control.Applicative ((*>))
import Data.Bits ((.|.))
import Data.Function (($))
import Data.Monoid ((<>), mconcat)
import Data.Ord ((<=))
import Data.String (String)
import System.IO (IO)

import Data.Map (Map)
import qualified Data.Map as Map (fromList)
import XMonad
    ( KeyMask
    , KeySym
    , X
    , XConfig
        ( XConfig
        , keys
        , layoutHook
        , logHook
        , manageHook
        , modMask
        , terminal
        , workspaces
        )
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
import qualified XMonad.Operations as Operations (windows)
import qualified XMonad.StackSet as StackSet
    ( focusDown
    , focusUp
    , greedyView
    , shift
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

--myKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys :: XConfig a -> Map (KeyMask, KeySym) (X ())
myKeys cfg@XConfig{modMask} = runCommands <> workspaceKeys <> switchWindowFocus
  where
    -- mod-r        -- Run command.
    -- shift-mod-l  -- Lock screen.
    runCommands = Map.fromList
        [ ((modMask, xK_r), spawn "xfce4-appfinder --collapsed")
        , ((modMask .|. shiftMask, xK_l), spawn "xflock4")
            -- Command xflock4 is a shell script that invokes correct locking
            -- application, if available.
        ]

    -- mod-[0..9] -- Switch to workspace N.
    -- alt-[1..9] -- Move client to workspace N.
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

    -- alt-tab       -- Focus next window.
    -- alt-shift-tab -- Focus previous window.
    switchWindowFocus = Map.fromList
        [ ((m, xK_Tab), Operations.windows f)
        | (m, f) <-
            [ (mod1Mask,               StackSet.focusDown)
            , (mod3Mask,               StackSet.focusDown)
            , (mod1Mask .|. shiftMask, StackSet.focusUp  )
            , (mod3Mask .|. shiftMask, StackSet.focusUp  )
            ]
        ]
