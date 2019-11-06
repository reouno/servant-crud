module Main where

import           Network.Wai.Handler.Warp

import           Lib
import           SampleServerDB           ( server )
import           SampleServerIMDB         ( app )

main :: IO ()
-- main = run 8081 =<< app -- for SampleServerIMDB
main = server
