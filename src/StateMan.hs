{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module StateMan where

import Control.Monad (filterM, (>=>))
import Data.ByteString (toStrict)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (for_)
import Data.List (elemIndex, sort)
import Data.Map (insert)
import Data.Maybe (fromMaybe)
import Display (get_new_window_lower, get_window_from_bound_and_height, scroll_off, terminal_display_region)
import qualified GHC.IO.Device as IOD
import System.Directory (copyFile, doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory, removeFile)
import System.FilePath (combine, replaceDirectory, takeBaseName, takeDirectory, (</>))
import Types
import UserInput
import Util (get_cursor_index_at_view, get_next_item, get_next_item_abs, get_window_lb_at_view, partition_directory_contents)

move_cursor :: MyAppState -> CMotions -> IO MyAppState
move_cursor mas@MyAppState {indices, viewing_path, visible_directories = (_, cc, _)} ca =
  update_window_lower >=> update_visible_directories $ case ca of
    CLeft -> mas {viewing_path = takeDirectory viewing_path}
    CRight -> case get_next_item mas of
      -- TODO:
      -- This is presumably when we'd open the file
      (IOD.RegularFile, bs) -> mas -- TODO: Open the file
      (_, next_dir_name) -> mas {viewing_path = viewing_path </> next_dir_name}
    CUp -> mas {indices = insert viewing_path (max 0 $ get_cursor_index_at_view viewing_path indices - 1) indices}
    CDown -> mas {indices = insert viewing_path (min (length cc - 1) $ get_cursor_index_at_view viewing_path indices + 1) indices}

manage_selection :: MyAppState -> CSelection -> IO MyAppState
manage_selection mas@MyAppState {selected, viewing_path} cs =
  update_visible_directories =<< case cs of
    Yank -> pure $ case selected of
      Selected fs -> mas {selected = Yanked fs}
      _ -> mas {selected = Yanked [snd $ get_next_item_abs mas]}
    DYank -> pure $ case selected of
      Selected fs -> mas {selected = DYanked fs}
      _ -> mas {selected = DYanked [snd $ get_next_item_abs mas]}
    SelectItem -> pure $ mas {selected = Selected [snd $ get_next_item_abs mas]}
    Put -> case selected of
      Yanked fs -> do
        for_ fs (\fp -> copyFile fp $ replaceDirectory fp viewing_path)
        pure $ mas {selected = NoneSelected}
      DYanked fs -> do
        for_ fs (\fp -> copyFile fp $ replaceDirectory fp viewing_path)
        for_ fs removeFile
        pure $ mas {selected = NoneSelected}
      _ -> pure mas

update_window_lower :: MyAppState -> IO MyAppState
update_window_lower mas@MyAppState {windows, visible_directories = (_, cc, _), viewing_path, my_vty, indices} = do
  (_, w_h) <- terminal_display_region my_vty
  let next_lower =
        ( get_new_window_lower
            (get_cursor_index_at_view viewing_path indices)
            scroll_off
            (length cc - 1)
            (get_window_from_bound_and_height (get_window_lb_at_view viewing_path windows) (w_h - 1))
        )
  return $
    mas
      { windows =
          insert
            viewing_path
            next_lower
            windows
      }

update_visible_directories :: MyAppState -> IO MyAppState
update_visible_directories mas@MyAppState {indices, viewing_path} = do
  -- TODO:
  -- If current directory updates and the current index doesn't match with the previously hovered item, might be nice to
  -- search and find the item, and update the index to the new location of that item
  curr_dir_conts <- directory_contents_alphab viewing_path

  -- TODO:
  -- Fix out of bounds indexing that occurs here
  let (next_item_type, next_item) = curr_dir_conts !! get_cursor_index_at_view viewing_path indices
      next_item_abs_path = viewing_path </> next_item
      -- TODO:
      -- Handle the case of `root`
      up_dir_path = takeDirectory viewing_path

  prev_dir_conts <- directory_contents_alphab up_dir_path
  next_item_conts <- do
    case next_item_type of
      -- Bytes read are limited to `max_bytes`
      IOD.RegularFile -> File . toStrict . LBS.take max_bytes <$> LBS.readFile next_item_abs_path
      _ -> Directory <$> directory_contents_alphab next_item_abs_path

  -- TODO:
  -- If we're on `root`, then previous directory is `Nothing`
  pure $
    mas
      { indices = insert up_dir_path (fromMaybe 0 $ elemIndex (IOD.Directory, takeBaseName viewing_path) prev_dir_conts) indices,
        visible_directories = (Just prev_dir_conts, curr_dir_conts, next_item_conts)
      }

directory_contents_alphab :: FilePath -> IO DirectoryContents
directory_contents_alphab fp = do
  dir_conts <- listDirectory fp
  d_directories <- filterM (doesDirectoryExist . combine fp) dir_conts
  d_files <- filterM (doesFileExist . combine fp) dir_conts
  return $ ((IOD.Directory,) <$> sort d_directories) ++ ((IOD.RegularFile,) <$> sort d_files)

clear_debug_msg :: MyAppState -> MyAppState
clear_debug_msg mas = mas {debug_msg = ""}

put_debug_msg_sep :: MyAppState -> String -> Char -> MyAppState
put_debug_msg_sep mas@MyAppState {debug_msg} s sep = mas {debug_msg = debug_msg ++ sep : s}

put_debug_msgs_sep :: (Show a) => MyAppState -> [a] -> Char -> MyAppState
put_debug_msgs_sep mas xs = put_debug_msg_sep mas (concatMap ((++) " " . show) xs)
