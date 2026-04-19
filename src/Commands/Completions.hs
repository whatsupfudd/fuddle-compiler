module Commands.Completions where

import qualified Options.Runtime as Rto
import qualified Options.Cli.Types as Ct

completionsCmd :: Ct.CompletionsOptions -> Rto.RunOptions -> IO ()
completionsCmd opts rtOpts =
  putStrLn "@[completionsCmd] starting."
