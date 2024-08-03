{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE NamedFieldPuns #-}

module Temp where

import Control.Monad (filterM)
import Control.Monad.Trans.State (State)
import Data.Bool (bool)
import Data.List (intersperse, sort)
import Data.Map (Map, empty, findWithDefault)
import Data.Maybe (fromMaybe)
import Display
import GHC.Float (float2Int, int2Float)
import GHC.IO.Device (IODeviceType)
import Graphics.Vty (Attr (Attr, attrBackColor, attrForeColor, attrStyle, attrURL), Color (Color240), DisplayRegion, Image, MaybeDefault (Default), Vty (Vty, nextEvent, outputIface, shutdown, update), black, blue, bold, char, charFill, cropRight, defAttr, defaultConfig, emptyImage, horizCat, picForImage, resizeWidth, string, translateX, vertCat, vertJoin, white, withBackColor, withForeColor, withStyle)
import Graphics.Vty.Output (Output (displayBounds))
import Graphics.Vty.Platform.Unix (mkVty)
import StateMan (clear_debug_msg, manage_selection, move_cursor, update_visible_directories)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory)
import System.FilePath (combine, takeDirectory, (</>))
import Types
import UserInput
import Util (get_cursor_index_at_view, get_next_item, partition_directory_contents)

app :: MyAppState -> IO ()
app as@MyAppState {my_vty} = do
  pic <- draw_app as
  update my_vty pic
  va <- nextVAction my_vty

  case va of
    CAction ca -> do
      move_cursor as ca >>= app
    CSelect cs -> do
      manage_selection as cs >>= app
    ClearDebug -> app $ clear_debug_msg as
    VQuit -> shutdown my_vty

get_initial_state :: IO MyAppState
get_initial_state = do
  vty <- mkVty defaultConfig
  pwd <- getCurrentDirectory

  -- TODO:
  -- I don't want to initialize the state like this
  update_visible_directories $ MyAppState {debug_msg = "", my_vty = vty, viewing_path = pwd, indices = empty, windows = empty, visible_directories = (Nothing, [], Directory []), selected = NoneSelected}

nextVAction :: Vty -> IO VAction
nextVAction v = do
  e <- nextEvent v
  case event_to_VAction e of
    Just va -> pure va
    Nothing -> nextVAction v
