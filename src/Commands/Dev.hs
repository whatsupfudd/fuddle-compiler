module Commands.Dev where

import qualified Options.Runtime as Rto
import qualified Options.Cli.Types as Ct

devCmd :: Ct.DevOptions -> Rto.RunOptions -> IO ()
devCmd opts rtOpts =
  putStrLn "@[devCmd] starting."
