module Commands.Explain where

import qualified Options.Runtime as Rto
import qualified Options.Cli.Types as Ct

explainCmd :: Ct.ExplainOptions -> Rto.RunOptions -> IO ()
explainCmd opts rtOpts =
  putStrLn "@[explainCmd] starting."
