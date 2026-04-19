module Commands.Doctor where

import qualified Options.Runtime as Rto
import qualified Options.Cli.Types as Ct

doctorCmd :: Ct.DoctorOptions -> Rto.RunOptions -> IO ()
doctorCmd opts rtOpts =
  putStrLn "@[doctorCmd] starting."
