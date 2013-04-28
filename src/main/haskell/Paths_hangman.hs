module Paths_hangman where

import Control.Monad

-- This module helps resolve data filepaths for development
-- see http://neilmitchell.blogspot.com.au/2008/02/adding-data-files-using-cabal.html
getDataFileName :: FilePath -> IO FilePath
getDataFileName = return . ("src/main/resources/" ++)
