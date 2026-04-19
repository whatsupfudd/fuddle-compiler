module Main where

import qualified Control.Exception as Cexc
import Control.Monad (void)

import qualified System.Environment as Senv
import qualified System.IO.Error as Serr

import qualified Options.Cli as Opt
import qualified Options.Cli.Base as Cli
import Options.Cli.Types (CliArgs(..), GlobalOptions(..))
import qualified Options.ConfFile as Opt
import qualified MainLogic as Ml


main :: IO ()
main = do
  -- eiOptions <- Opt.parseCliOptions
  eiOptions <- Senv.getArgs >>= Cli.parseCliArgs
  case eiOptions of
    Left errMsg -> putStrLn $ "@[main] err: " <> errMsg
    Right cliArgs -> do
      mbFileOptions <- case cliArgs.globals.configFiles of
        [] -> do
          eiEnvConfFile <- Cexc.try $ Senv.getEnv "fuddleCONF" :: IO (Either Serr.IOError String)
          case eiEnvConfFile of
            Left _ -> do
              eiConfPath <- Opt.defaultConfigFilePath
              case eiConfPath of
                Left err -> pure . Left $ "@[main] defaultConfigFilePath err: " <> err
                Right confPath -> Opt.parseFileOptions confPath
            Right aPath -> Opt.parseFileOptions aPath
        aPath : _ -> Opt.parseFileOptions aPath
      case mbFileOptions of
        Left errMsg -> putStrLn $ "@[main] err: " <> errMsg
        Right fileOptions ->
          void $ Ml.runWithOptions cliArgs fileOptions
