module Commands.Build where

import qualified Options.Runtime as Rto
import qualified Options.Cli.Types as Ct

buildCmd :: Ct.BuildOptions -> Rto.RunOptions -> IO ()
buildCmd opts rtOpts =
  putStrLn "@[buildCmd] starting."
