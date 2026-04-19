module MainLogic
where

import Data.Text (pack)
import qualified System.Environment as Env

import qualified Options as Opt
import qualified Options.Cli as Opt (CliOptions (..), EnvOptions (..), Command (..))
import qualified Options.Cli.Types as Ct
import qualified Options.ConfFile as Opt (FileOptions (..))
import qualified Options.Runtime as Rt (defaultRun)
import Commands as Cmd


runWithOptions :: Ct.CliArgs -> Opt.FileOptions -> IO ()
runWithOptions cliArgs fileOptions = do
  -- putStrLn $ "@[runWithOptions] cliOpts: " <> show cliOptions
  -- putStrLn $ "@[runWithOptions] fileOpts: " <> show fileOptions
  -- Get environmental context in case it's required in the merge. Done here to keep the merge pure:
  mbHome <- Env.lookupEnv "fuddleHOME"
  let
    envOptions = Opt.EnvOptions {
        Opt.appHome = mbHome
        -- TODO: put additional env vars.
      }
    -- switchboard to command executors:
    cmdExecutor =
      case cliArgs.command of
        Ct.BuildCmd opts -> Cmd.buildCmd opts
        Ct.CheckCmd opts -> Cmd.checkCmd opts
        Ct.DevCmd opts -> Cmd.devCmd opts
        Ct.PreviewCmd opts -> Cmd.previewCmd opts
        Ct.TestCmd opts -> Cmd.testCmd opts
        Ct.FmtCmd opts -> Cmd.fmtCmd opts
        Ct.ReplCmd opts -> Cmd.replCmd opts
        Ct.CleanCmd opts -> Cmd.cleanCmd opts
        Ct.InitCmd opts -> Cmd.initCmd opts
        Ct.NewCmd opts -> Cmd.newCmd opts
        Ct.DepsCmd opts -> Cmd.depsCmd opts
        Ct.MetadataCmd opts -> Cmd.metadataCmd opts
        Ct.InspectCmd opts -> Cmd.inspectCmd opts
        Ct.DoctorCmd opts -> Cmd.doctorCmd opts
        Ct.ExplainCmd opts -> Cmd.explainCmd opts
        Ct.CompletionsCmd opts -> Cmd.completionsCmd opts
        Ct.HelpCmd opts -> Cmd.helpCmd opts
        Ct.VersionCmd -> Cmd.versionCmd
  -- rtOptions <- Opt.mergeOptions cliOptions fileOptions envOptions
  let
    rtOptions = Rt.defaultRun
  result <- cmdExecutor rtOptions
  -- TODO: return a properly kind of conclusion.
  pure ()
