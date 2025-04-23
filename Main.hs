module Main (main) where

import Tipos
import Funcoes
import Persistencia
import System.IO
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)
import System.Directory (doesFileExist)
import Control.Monad (when)
import Text.Read (readMaybe)
import System.Exit (exitSuccess)

main :: IO ()
main = do 
         menu []
