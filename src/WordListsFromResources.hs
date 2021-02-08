module WordListsFromResources where

import Data.String.Utils
import System.Directory

-- Returns paths for h
getWordListsFromResources :: IO [[String]]
getWordListsFromResources = filter (endsWith ".txt") (getDirectoryContents "resources/wordlists")