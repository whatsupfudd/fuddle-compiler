module Commands.Inspect where

import qualified Options.Runtime as Rto
import qualified Options.Cli.Types as Ct

inspectCmd :: Ct.InspectCommand -> Rto.RunOptions -> IO ()
inspectCmd opts rtOpts =
  putStrLn "@[inspectCmd] starting."
