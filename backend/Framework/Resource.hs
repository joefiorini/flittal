module Framework.Resource
( Resource(..)
, ResourceStore
, getResourceList
)
where

import Data.Text.Lazy (Text)
import qualified Database.PostgreSQL.Simple as DB

getResourceList :: (DB.FromRow a) => DB.Query -> ResourceStore -> IO [a]
getResourceList q c = (DB.query_ c q)

type ResourceStore = DB.Connection

data Resource a = Resource
  { title :: Text
  , list :: ResourceStore -> IO [a]
  }

