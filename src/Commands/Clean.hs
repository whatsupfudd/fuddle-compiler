module Commands.Clean where

import qualified Options.Runtime as Rto
import qualified Options.Cli.Types as Ct

cleanCmd :: Ct.CleanOptions -> Rto.RunOptions -> IO ()
cleanCmd opts rtOpts =
  putStrLn "@[cleanCmd] starting."
