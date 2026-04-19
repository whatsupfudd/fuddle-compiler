module Commands.Metadata where

import qualified Options.Runtime as Rto
import qualified Options.Cli.Types as Ct

metadataCmd :: Ct.MetadataOptions -> Rto.RunOptions -> IO ()
metadataCmd opts rtOpts =
  putStrLn "@[metadataCmd] starting."
