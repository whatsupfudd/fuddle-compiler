module Commands.New where

import qualified Options.Runtime as Rto
import qualified Options.Cli.Types as Ct

newCmd :: Ct.NewOptions -> Rto.RunOptions -> IO ()
newCmd opts rtOpts =
  putStrLn "@[newCmd] starting."
