module Api.User (resource) where

import Control.Concurrent.STM (readTVar, atomically)
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.Trans (liftIO)
import Data.Set (Set)
import qualified Data.Set         as Set
import qualified Data.Text        as T

import Rest (Handler, ListHandler, Range (count, offset), Resource, Void, mkListing, mkResourceReader, named, singleRead, withListing, xmlJsonO)
import qualified Rest.Resource    as R

import ApiTypes (Diagrammer, ServerData (..))
import Type.User (User)
import Type.UserInfo (UserInfo(UserInfo))
import qualified Type.User        as User
import qualified Type.UserInfo    as UserInfo

type WithUser = ReaderT User.Name Diagrammer

resource :: Resource Diagrammer WithUser User.Name () Void
resource = mkResourceReader
  { R.name = "user"
  , R.schema = withListing () $ named [("name", singleRead id)]
  , R.list    = const list
  -- , R.create  = Just create
  }

list :: ListHandler Diagrammer
list = mkListing xmlJsonO $ \r -> do
  usrs <- liftIO . atomically . readTVar =<< asks users
  return . map toUserInfo . take (count r) . drop (offset r) . Set.toList $ usrs

toUserInfo :: User -> UserInfo
toUserInfo u = UserInfo { UserInfo.name = User.name u }
