module Commands.Test where

import qualified Options.Runtime as Rto
import qualified Options.Cli.Types as Ct

testCmd :: Ct.TestOptions -> Rto.RunOptions -> IO ()
testCmd opts rtOpts =
  putStrLn "@[testCmd] starting."
