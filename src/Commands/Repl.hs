module Commands.Repl where

import qualified Options.Runtime as Rto
import qualified Options.Cli.Types as Ct

replCmd :: Ct.ReplOptions -> Rto.RunOptions -> IO ()
replCmd opts rtOpts =
  putStrLn "@[replCmd] starting."
