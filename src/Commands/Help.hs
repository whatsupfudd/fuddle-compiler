module Commands.Help where

import qualified Options.Runtime as Rto
import qualified Options.Cli.Types as Ct

helpCmd :: Ct.HelpOptions -> Rto.RunOptions -> IO ()
helpCmd opts rtOpts =
  putStrLn "@[helpCmd] starting."
