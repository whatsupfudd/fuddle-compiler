module Commands.Native where

import qualified Options.Runtime as Rto
import qualified Options.Cli.Types as Ct

nativeCmd :: Ct.NativeCommand -> Rto.RunOptions -> IO ()
nativeCmd opts rtOpts =
  putStrLn "@[nativeCmd] starting."
