{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Network.Wai.Handler.Warp
import Rest.Driver.Wai (apiToApplication)

import Api (api)
import ApiTypes (runApi)
import Diagrammer (diagrammer)

main :: IO ()
main = do

  -- Set up the server state for the blog and start warp.
  putStrLn "Starting warp server on http://localhost:3000"
  serverData <- diagrammer
  run 3000 $
    apiToApplication (runApi serverData) api
