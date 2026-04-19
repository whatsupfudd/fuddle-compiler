module Commands.Init where

import qualified Options.Runtime as Rto
import qualified Options.Cli.Types as Ct

initCmd :: Ct.InitOptions -> Rto.RunOptions -> IO ()
initCmd opts rtOpts =
  putStrLn "@[initCmd] starting."
