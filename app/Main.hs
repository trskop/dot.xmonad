{-# LANGUAGE NoImplicitPrelude #-}
module Main (main)
  where

--import Data.Function (($))
import System.IO (IO)

import XMonad
    ( XConfig(modMask)
    , mod4Mask
    , xmonad
    )
import XMonad.Config.Xfce (xfceConfig)

import Local.XMonad ()


main :: IO ()
main = xmonad xfceConfig
    { modMask = mod4Mask
    }
