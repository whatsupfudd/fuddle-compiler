module Commands.Check where

import qualified Options.Runtime as Rto
import qualified Options.Cli.Types as Ct

checkCmd :: Ct.CheckOptions -> Rto.RunOptions -> IO ()
checkCmd opts rtOpts =
  putStrLn "@[checkCmd] starting."
