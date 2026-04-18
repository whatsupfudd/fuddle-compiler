module Options.Runtime (defaultRun, RunOptions (..), PgDbConfig (..), defaultPgDbConf) where
-- import Data.Int (Int)

import Data.Text (Text)

import DB.Connect (PgDbConfig (..), defaultPgDbConf)


data RunOptions = RunOptions {
    debug :: Int
    , pgDbConf :: PgDbConfig
    -- HERE: Add additional vars for providing runtime parameters:
    -- Eg: , root :: Text
  }
  deriving (Show)

defaultRun :: RunOptions
defaultRun =
  RunOptions {
    debug = 0
    , pgDbConf = defaultPgDbConf
   -- HERE: Set default value for additional runtime parameters:  , root = "/tmp"
  }
