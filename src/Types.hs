{-# HLINT ignore "Use camelCase" #-}

module Types where

import Control.Monad.Trans.State (State)
import Data.ByteString (ByteString)
import Data.Map (Map)
import GHC.IO.Device (IODeviceType (RegularFile))
import GHC.Int (Int64)
import Graphics.Vty (Vty)

max_bytes :: Int64
max_bytes = 1000

data IODevContent = Directory DirectoryContents | File ByteString

-- The `FilePath` is expressed as a relative path to the current directory (so it's just the file / subdirectory name without any path delimiters)
type DirectoryContents = [(IODeviceType, FilePath)]

type CursorPos = Int

type Window = (Int, Int)

-- TODO:
-- implement `Default` typeclass
data MyAppState = MyAppState
  { my_vty :: Vty,
    viewing_path :: FilePath, -- The currently "viewed" absolute path
    indices :: Map FilePath CursorPos,
    -- The index of the first item in view of the window. The upper bound is implicitly just (lower_bound + window_height) the index of the first item plus the window size
    windows :: Map FilePath Int,
    -- Previous directory is `Maybe` because the current directory can be `root`
    visible_directories :: (Maybe DirectoryContents, DirectoryContents, IODevContent),
    selected :: SelectionType,
    -- NOTE: Temporary solution to debugging
    debug_msg :: String
  }

data SelectionType = NoneSelected | Selected [FilePath] | Yanked [FilePath] | DYanked [FilePath]

type MyMState = State MyAppState

-- TODO:
-- Delete it or newtype wrap it
instance Show IODeviceType where
  show RegularFile = "File"
  show _ = "Directory"
