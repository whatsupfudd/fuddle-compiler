module Commands.Fmt where

import qualified Options.Runtime as Rto
import qualified Options.Cli.Types as Ct

fmtCmd :: Ct.FmtOptions -> Rto.RunOptions -> IO ()
fmtCmd opts rtOpts =
  putStrLn "@[fmtCmd] starting."
