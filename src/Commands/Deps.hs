module Commands.Deps where

import qualified Options.Runtime as Rto
import qualified Options.Cli.Types as Ct

depsCmd :: Ct.DepsCommand -> Rto.RunOptions -> IO ()
depsCmd opts rtOpts =
  putStrLn "@[depsCmd] starting."
