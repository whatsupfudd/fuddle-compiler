module Commands.Preview where

import qualified Options.Runtime as Rto
import qualified Options.Cli.Types as Ct

previewCmd :: Ct.PreviewOptions -> Rto.RunOptions -> IO ()
previewCmd opts rtOpts =
  putStrLn "@[previewCmd] starting."
