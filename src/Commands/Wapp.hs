module Commands.Wapp where

import qualified Options.Runtime as Rto
import qualified Options.Cli.Types as Ct

wappCmd :: Ct.WappCommand -> Rto.RunOptions -> IO ()
wappCmd opts rtOpts =
  putStrLn "@[wappCmd] starting."
