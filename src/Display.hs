{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE NamedFieldPuns #-}

module Display where

import Data.Bool (bool)
import Data.List (intersperse, sort)
import Data.Maybe (fromMaybe)
import GHC.Float (float2Int, int2Float)
import Graphics.Vty (Attr (Attr, attrBackColor, attrForeColor, attrStyle, attrURL), Color (Color240), DisplayRegion, Image, MaybeDefault (Default), Picture, Vty (Vty, nextEvent, outputIface, shutdown, update), black, blue, bold, char, charFill, cropRight, defAttr, defaultConfig, emptyImage, horizCat, picForImage, picForLayers, resizeWidth, string, translateX, vertCat, vertJoin, white, withBackColor, withForeColor, withStyle)
import Graphics.Vty.Output (Output (displayBounds))
import System.FilePath (combine, takeDirectory, (</>))
import Types
import Util (get_cursor_index_at_view, get_next_item, get_window_lb_at_view, partition_directory_contents)

left_panel_percent, center_panel_percent, right_panel_percent :: Float
left_panel_percent = 0.2
center_panel_percent = 0.40
right_panel_percent = 0.50

panel_padding, scroll_off :: Int
panel_padding = 2
scroll_off = 5

pure_black :: Color
pure_black = Color240 0

-- NOTE:
-- We don't need to provide the surrounding region height. The
-- Given cursor position, scroll-off, the max window point, and the current window view returns the new window view that correctly binds the window
get_new_window_lower :: CursorPos -> Int -> Int -> Window -> Int
get_new_window_lower cp sf max_items (w_lower, w_upper)
  | cp_buff_low < w_lower = cp_buff_low
  | cp_buff_upp > w_upper = w_lower + (cp_buff_upp - w_upper)
  | otherwise = w_lower
  where
    (cp_buff_low, cp_buff_upp) = (max 0 $ cp - sf, min max_items $ cp + sf)

-- get_new_window_lower :: CursorPos -> Int -> Int -> Window -> Int
-- get_new_window_lower cp sf max_items (w_lower, w_upper) =
--   let (view_lower, view_upper) = (max 0 $ cp - sf, min max_items $ cp + sf)
--       delta_win = max (-w_lower) (min 0 view_lower - w_lower) + min (max_items - w_upper) (max 0 view_upper - w_upper)
--    in w_lower + delta_win

-- get_new_window_lower :: CursorPos -> Int -> Window -> Int
-- get_new_window_lower cp max_items (w_lower, w_upper)
--   | cp < w_lower = cp
--   | cp > w_upper = w_lower + (cp - w_upper)
--   | otherwise = w_lower

get_window_from_bound_and_height :: Int -> Int -> Window
get_window_from_bound_and_height l h = (l, l + h)

-- Is non-inclusive on the upper bound. Works on 0-based indexing. (Ex. get_items_between (0, 3) [1..4] = [1, 2, 3])
get_items_between :: (Int, Int) -> [a] -> [a]
get_items_between (l, u) = drop l . take u

draw_app :: MyAppState -> IO Picture
draw_app mas@MyAppState {debug_msg} =
  -- NOTE:
  -- Here is where to enable debugging
  picForLayers <$> sequence [{-pure $ debug_image debug_msg, -} build_current_view_image mas]

-- img <- build_current_view_image mas
-- return $ picForImage img

-- TODO:
-- - Reduce computations
-- - Cursor location in other directories should be remembered in the state
-- - Cursor of previous directory should be at the currently opened directory
build_current_view_image :: MyAppState -> IO Image
build_current_view_image mas@MyAppState {my_vty, viewing_path, indices, windows, visible_directories = (prev_cont, curr_cont, next_cont)} = do
  (display_width, display_height) <- terminal_display_region my_vty

  -- Converts percent of display each panel should use into absolute widths. Accounts for padding by subtracting out the padding region
  -- TODO:
  -- Currently subtracts off padding from the right-most panel, which unnecessarily reduces the size of that panel
  let panel_widths = fmap ((-panel_padding +) . float2Int . (int2Float display_width *)) [left_panel_percent, center_panel_percent, right_panel_percent]

  -- TODO:
  -- Properly handle root directory case
  let prev_dc = fromMaybe [] prev_cont

  let prev_dir = takeDirectory viewing_path
  let next_dir = viewing_path </> snd (get_next_item mas)

  let prev_view_window = get_window_from_bound_and_height (get_window_lb_at_view prev_dir windows) display_height
  let curr_view_window = get_window_from_bound_and_height (get_window_lb_at_view viewing_path windows) display_height
  let next_view_window = get_window_from_bound_and_height (get_window_lb_at_view next_dir windows) display_height

  return $
    horizCat
    -- TODO:
    -- This is probably inefficient.
    -- It is here because doing simply `char` and resizing resulted in glitchy-looking cursor highlighting
    $
      intersperse (charFill defAttr ' ' panel_padding display_height) $
        zipWith
          resizeWidth
          panel_widths
          [ build_directory_image prev_view_window prev_dc (get_cursor_index_at_view prev_dir indices),
            build_directory_image curr_view_window curr_cont (get_cursor_index_at_view viewing_path indices),
            get_preview_image next_cont next_view_window $ get_cursor_index_at_view next_dir indices
          ]

debug_image :: String -> Image
debug_image s = vertCat $ string defAttr <$> lines s

get_preview_image :: IODevContent -> Window -> CursorPos -> Image
-- TODO:
-- - Actually preview file
-- - Don't show the entire file if it's text/string data. (Uses too much memory to load an entire file)
get_preview_image (File bytes) _ _ = string defAttr $ show bytes
get_preview_image (Directory dc) w i = build_directory_image w dc i

-- WARN:
-- This is not type safe (type alias is the same type)
-- TODO:
-- Cursor highlighting technique is giga-sus
build_directory_image :: Window -> DirectoryContents -> CursorPos -> Image
build_directory_image w@(w_low, _) dc i =
  let (dirs, files) = partition_directory_contents $ get_items_between w dc
      dir_attr = defAttr `withStyle` bold `withForeColor` blue
      dir_cursor_attr = defAttr `withStyle` bold `withBackColor` blue `withForeColor` pure_black
      files_attr = defAttr `withForeColor` white
      files_cursor_attr = defAttr `withForeColor` pure_black `withBackColor` white
      num_dirs = length dirs
      dirs_img = ((\(ind, x) -> string (bool dir_attr dir_cursor_attr (ind == (i - w_low))) x) <$> zip [0 .. num_dirs] dirs)
      files_img = ((\(ind, x) -> string (bool files_attr files_cursor_attr ((ind + num_dirs) == (i - w_low))) x) <$> zip [0 .. length files] files)
   in vertCat (dirs_img ++ files_img)

-- The file/dir that the cursor is on turns the text `black` and for background as the original text color.
cursor_highlight :: Attr -> Attr
cursor_highlight Attr {attrStyle = sty, attrForeColor = fc, attrURL = url} =
  Attr
    { attrURL = url,
      attrStyle = sty,
      attrForeColor = Default,
      attrBackColor = fc
    }
    `withForeColor` black

terminal_display_region :: Vty -> IO DisplayRegion
terminal_display_region = displayBounds . outputIface
