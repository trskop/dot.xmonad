{-# LANGUAGE NamedFieldPuns #-}
module Local.XMonad
  where

import Data.Function (($))
import qualified Data.List as List (concatMap, zip)
import Data.String (String)
import Data.Tuple (uncurry)

import Data.Map (Map)
import qualified Data.Map as Map (fromList)
import XMonad
    ( KeyMask
    , KeySym
    , X
    , XConfig(XConfig, workspaces)
    )


workspaceKeys
    :: (Num idx, Enum idx)
    => XConfig a
    -> (XConfig a -> String -> idx -> [((KeyMask, KeySym), X ())])
    -> Map (KeyMask, KeySym) (X ())
workspaceKeys cfg@XConfig{workspaces} f =
    Map.fromList . List.concatMap (uncurry $ f cfg) $ List.zip workspaces [0..]
