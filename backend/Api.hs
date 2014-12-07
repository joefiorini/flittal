module Api where

import Rest.Api

import ApiTypes (Diagrammer)
import qualified Api.User         as User


api :: Api Diagrammer
api = [(mkVersion 1 0 0, Some1 routes)]

routes :: Router Diagrammer Diagrammer
routes =
  root -/ user
  where
    user        = route User.resource
