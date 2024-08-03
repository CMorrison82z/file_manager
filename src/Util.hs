{-# LANGUAGE NamedFieldPuns #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE TupleSections #-}

module Util where

import Control.Monad (filterM)
import Control.Monad.Trans.State (State)
import Data.Bool (bool)
import Data.List (intersperse, partition, sort)
import Data.Map (Map, empty, findWithDefault)
import Debug.Trace (trace)
import GHC.Float (float2Int, int2Float)
import GHC.IO.Device (IODeviceType (Directory, RegularFile))
import Graphics.Vty (Attr (Attr, attrBackColor, attrForeColor, attrStyle, attrURL), Color (Color240), DisplayRegion, Image, MaybeDefault (Default), Vty (Vty, nextEvent, outputIface, shutdown, update), black, blue, bold, char, cropRight, defAttr, defaultConfig, emptyImage, horizCat, picForImage, resizeWidth, string, translateX, vertCat, vertJoin, white, withBackColor, withForeColor, withStyle)
import Graphics.Vty.Output (Output (displayBounds))
import Graphics.Vty.Platform.Unix (mkVty)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory)
import System.FilePath (combine, takeDirectory, (</>))
import Types hiding (Directory)

get_next_item :: MyAppState -> (IODeviceType, FilePath)
get_next_item MyAppState {indices, viewing_path, visible_directories = (_, cc, _)} =
  cc !! get_cursor_index_at_view viewing_path indices

-- Returns next item as absolute path
get_next_item_abs :: MyAppState -> (IODeviceType, FilePath)
get_next_item_abs MyAppState {indices, viewing_path, visible_directories = (_, cc, _)} =
  fmap (viewing_path </>) $ cc !! get_cursor_index_at_view viewing_path indices

partition_directory_contents :: DirectoryContents -> ([FilePath], [FilePath])
partition_directory_contents dc =
  let (d, f) = partition ((==) Directory . fst) dc
   in (fmap snd d, fmap snd f)

get_cursor_index_at_view :: FilePath -> Map FilePath CursorPos -> CursorPos
get_cursor_index_at_view = findWithDefault 0

get_window_lb_at_view :: FilePath -> Map FilePath Int -> Int
get_window_lb_at_view = findWithDefault 0
