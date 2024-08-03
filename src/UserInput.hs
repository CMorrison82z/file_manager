{-# HLINT ignore "Use camelCase" #-}

module UserInput where

import Control.Applicative ((<|>))
import Graphics.Vty (Event (EvKey), Key (KChar))
import System.FilePath (takeDirectory)
import Types

-- NOTE:
-- Consider Changing Mode to be a `Command` in context of the "number, motion, command" paradigm.
-- Could also be cool to think of things like `a` for "append" to be interpreted as a `motion` + `command` (a ChangeMode command as mentioned above)
data VAction = VQuit | CAction CMotions | ClearDebug | CSelect CSelection

data CMotions = CUp | CDown | CLeft | CRight

data CSelection = Yank | DYank | SelectItem | Put

-- CUp -> (nextRow, min (max 0 (length (b !! nextRow) - 1)) c) where nextRow = max 0 (r - 1)
-- CDown -> (nextRow, min (max 0 (length (b !! nextRow) - 1)) c) where nextRow = min (length b) (r + 1)
-- CLeft -> (r, max 0 (c - 1))
-- CRight -> (r, min (max 0 (length (b !! r) - 1)) (c + 1))

event_to_cursor_dir :: Event -> Maybe CMotions
event_to_cursor_dir (EvKey (KChar k) []) = case k of
  'h' -> Just CLeft
  'j' -> Just CDown
  'k' -> Just CUp
  'l' -> Just CRight
  _ -> Nothing
event_to_cursor_dir _ = Nothing

event_to_selection :: Event -> Maybe CSelection
event_to_selection (EvKey (KChar k) []) = case k of
  'y' -> Just Yank
  'd' -> Just DYank
  ' ' -> Just SelectItem
  'p' -> Just Put
  _ -> Nothing
event_to_selection _ = Nothing

event_to_VAction :: Event -> Maybe VAction
event_to_VAction e@(EvKey k []) =
  (CAction <$> event_to_cursor_dir e)
    <|> (CSelect <$> event_to_selection e)
    <|> ( case k of
            KChar 'q' -> Just VQuit
            -- TODO: This is temporary
            KChar 'c' -> Just ClearDebug
            _ -> Nothing
        )
event_to_VAction _ = Nothing

-- Grab up to and including a motion. It may contain a "count" and a "command". No command is implicitly a movement
-- get_command_thing :: [Stuff] -> ???
-- get_command_thing l = takeUntil is_command
