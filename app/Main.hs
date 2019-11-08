module Main where

import           Network.Wai.Handler.Warp

import           Lib
import qualified SampleServerDB           as Server
import qualified SampleServerIMDB         as ServerIM

main :: IO ()
-- main = run 8081 =<< ServerIM.app -- for SampleServerIMDB
main = run 8081 =<< Server.app
