module Main where

import           Network.Wai.Handler.Warp

import           Lib
import           SampleServer             ( app )

main :: IO ()
main = run 8081 =<< app
