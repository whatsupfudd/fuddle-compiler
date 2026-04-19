
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Options.Cli.Base ( parseCliArgs, cliInfo ) where

import Control.Applicative ((<**>), (<|>), many, optional, some)
import Data.Char (isSpace, toLower)
import Data.List (intercalate, isInfixOf, stripPrefix)
import qualified Data.Set as Set
import qualified Options.Applicative as OA
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.FilePath ((</>), isRelative, normalise, takeDirectory, takeExtension)

import Options.Cli.Types


--------------------------------------------------------------------------------
-- Public entry points
--------------------------------------------------------------------------------

parseCliArgs :: [String] -> IO Cli
parseCliArgs args = do
  argsExpanded <- expandResponseFilesIO args
  OA.handleParseResult $
    OA.execParserPure cliPrefs cliInfo argsExpanded

cliInfo :: OA.ParserInfo Cli
cliInfo =
  OA.info (cliParser <**> OA.helper) ( 
         OA.fullDesc
      <> OA.header "fuddle - Fuddle compiler and EasyWordy build tool"
      <> OA.progDesc "Compile Fuddle projects, drive dev workflows, inspect compiler internals, and stage/install EasyWordy Wapps."
    )

cliPrefs :: OA.ParserPrefs
cliPrefs = OA.prefs $ OA.showHelpOnEmpty <> OA.showHelpOnError

--------------------------------------------------------------------------------
-- Top-level parser
--------------------------------------------------------------------------------

cliParser :: OA.Parser Cli
cliParser =
  Cli
    <$> globalOptionsParser
    <*> ( versionFlagParser
      <|> commandParser
      <|> shorthandBuildParser
        )

versionFlagParser :: OA.Parser Command
versionFlagParser =
  OA.flag'
    VersionCmd
    ( OA.short 'V'
   <> OA.long "version"
   <> OA.help "Show compiler version and exit"
    )

commandParser :: OA.Parser Command
commandParser =
  OA.hsubparser $
       visibleCommand "build" (BuildCmd <$> buildOptionsParser EntryOptional) "Compile and produce artifacts"
    <> hiddenCommand "make" (BuildCmd <$> buildOptionsParser EntryOptional) "Alias for build"

    <> visibleCommand "check" (CheckCmd <$> checkOptionsParser) "Fast semantic validation without normal artifact emission"
    <> visibleCommand "dev" (DevCmd <$> devOptionsParser) "Watch, rebuild, and run the local development loop"
    <> hiddenCommand "watch" (DevCmd <$> devOptionsParser) "Alias for dev"
    <> visibleCommand "preview" (PreviewCmd <$> previewOptionsParser) "Serve already-built production artifacts locally"
    <> visibleCommand "test" (TestCmd <$> testOptionsParser) "Run project tests"
    <> visibleCommand "fmt" (FmtCmd <$> fmtOptionsParser) "Format Fuddle source files"
    <> visibleCommand "repl" (ReplCmd <$> replOptionsParser) "Launch the interactive shell"
    <> visibleCommand "clean" (CleanCmd <$> cleanOptionsParser) "Remove build outputs and/or caches"

    <> visibleCommand "init" (InitCmd <$> initOptionsParser) "Initialize the current directory"
    <> visibleCommand "new" (NewCmd <$> newOptionsParser) "Create a new project, workspace, or Wapp scaffold"
    <> visibleCommand "deps" (DepsCmd <$> depsCommandParser) "Dependency and lock/vendor operations"
    <> visibleCommand "metadata" (MetadataCmd <$> metadataOptionsParser) "Emit machine-readable project metadata"

    <> visibleCommand "inspect" (InspectCmd <$> inspectCommandParser) "Inspect CST, AST, symbols, graphs, IR, and JS output"
    <> hiddenCommand "graph" (InspectCmd <$> inspectGraphAliasParser) "Alias for inspect graph"
    <> hiddenCommand "ir" (InspectCmd <$> inspectIrAliasParser) "Alias for inspect ir"

    <> visibleCommand "doctor" (DoctorCmd <$> doctorOptionsParser) "Validate toolchain, environment, and project health"
    <> visibleCommand "explain" (ExplainCmd <$> explainOptionsParser) "Explain a diagnostic code in detail"
    <> visibleCommand "completions" (CompletionsCmd <$> completionsOptionsParser) "Generate shell completion scripts"
    <> visibleCommand "help" (HelpCmd <$> helpOptionsParser) "Show help for the CLI or a specific topic"
    <> visibleCommand "version" (pure VersionCmd) "Show compiler version"

    <> visibleCommand "wapp" (WappCmd <$> wappCommandParser) "Stage, install, and inspect EasyWordy Wapps"
    <> visibleCommand "native" (NativeCmd <$> nativeCommandParser) "Build and install native integrations"
  where
    visibleCommand :: String -> OA.Parser a -> String -> OA.Mod OA.CommandFields a
    visibleCommand name parser desc =
      OA.command name (OA.info parser (OA.progDesc desc))

    hiddenCommand :: String -> OA.Parser a -> String -> OA.Mod OA.CommandFields a
    hiddenCommand name parser desc =
      OA.command name (OA.info parser (OA.progDesc desc)) <> OA.hidden

shorthandBuildParser :: OA.Parser Command
shorthandBuildParser =
  BuildCmd <$> buildOptionsParser EntryRequired

--------------------------------------------------------------------------------
-- Global options
--------------------------------------------------------------------------------

globalOptionsParser :: OA.Parser GlobalOptions
globalOptionsParser =
  GlobalOptions
    <$> optional (OA.strOption
          ( OA.short 'C'
         <> OA.long "directory"
         <> OA.metavar "DIR"
         <> OA.help "Change working directory before doing anything else"
          ))
    <*> optional (OA.strOption
          ( OA.long "manifest-path"
         <> OA.metavar "FILE"
         <> OA.help "Use an explicit Fuddle project or workspace manifest"
          ))
    <*> verbosityParser
    <*> OA.switch
          ( OA.short 'q'
         <> OA.long "quiet"
         <> OA.help "Suppress non-essential output"
          )
    <*> OA.option (enumReader "color mode" colorModeMap)
          ( OA.long "color"
         <> OA.metavar "MODE"
         <> OA.value ColorAuto
         <> OA.help "Color mode: auto, always, never"
          )
    <*> OA.option (enumReader "report mode" reportModeMap)
          ( OA.long "report"
         <> OA.metavar "MODE"
         <> OA.value ReportHuman
         <> OA.help "Global report mode: human or json"
          )
    <*> many (OA.strOption
          ( OA.long "config"
         <> OA.metavar "FILE"
         <> OA.help "Additional config file"
          ))
    <*> many (OA.strOption
          ( OA.long "set"
         <> OA.metavar "KEY=VALUE"
         <> OA.help "Override a config key from the command line"
          ))
    <*> many (OA.strOption
          ( OA.short 'Z'
         <> OA.long "unstable"
         <> OA.metavar "FEATURE"
         <> OA.help "Enable an unstable feature gate"
          ))
    <*> OA.switch
          ( OA.long "no-progress"
         <> OA.help "Disable progress bars and spinners"
          )

verbosityParser :: OA.Parser Int
verbosityParser =
  length <$> many
    (OA.flag'
      ()
      ( OA.short 'v'
     <> OA.long "verbose"
     <> OA.help "Increase verbosity (repeatable)"
      ))

--------------------------------------------------------------------------------
-- Shared parsers
--------------------------------------------------------------------------------

memberSelectionParser :: OA.Parser MemberSelection
memberSelectionParser =
  MemberSelection
    <$> OA.switch
          ( OA.long "workspace"
         <> OA.help "Select all workspace members"
          )
    <*> many (OA.strOption
          ( OA.short 'p'
         <> OA.long "package"
         <> OA.metavar "NAME"
         <> OA.help "Select a workspace package (repeatable)"
          ))

selectionParser :: EntryRequirement -> OA.Parser Selection
selectionParser entryRequirement =
  Selection
    <$> many (OA.strOption
          ( OA.long "target"
         <> OA.metavar "NAME"
         <> OA.help "Select a named manifest target (repeatable)"
          ))
    <*> OA.switch
          ( OA.long "all-targets"
         <> OA.help "Select every declared target"
          )
    <*> ((<>) <$> many entryOptionParser <*> positionalEntriesParser entryRequirement)
    <*> memberSelectionParser

resolutionParser :: OA.Parser ResolutionOptions
resolutionParser =
  ResolutionOptions
    <$> optional (OA.option OA.auto
          ( OA.short 'j'
         <> OA.long "jobs"
         <> OA.metavar "N"
         <> OA.help "Maximum number of concurrent jobs"
          ))
    <*> OA.switch
          ( OA.long "keep-going"
         <> OA.help "Continue independent work after failures"
          )
    <*> OA.switch
          ( OA.long "offline"
         <> OA.help "Do not access the network"
          )
    <*> OA.switch
          ( OA.long "locked"
         <> OA.help "Require an up-to-date lockfile"
          )
    <*> OA.switch
          ( OA.long "frozen"
         <> OA.help "Equivalent to --locked and --offline"
          )
    <*> optional (OA.strOption
          ( OA.long "target-dir"
         <> OA.metavar "DIR"
         <> OA.help "Directory for build outputs"
          ))
    <*> optional (OA.strOption
          ( OA.long "cache-dir"
         <> OA.metavar "DIR"
         <> OA.help "Directory for compiler and incremental caches"
          ))

lintOptionsParser :: OA.Parser LintOptions
lintOptionsParser =
  LintOptions
    <$> OA.switch
          ( OA.long "warnings-as-errors"
         <> OA.help "Treat warnings as errors"
          )
    <*> many (OA.strOption
          ( OA.long "allow"
         <> OA.metavar "LINT"
         <> OA.help "Allow a lint by name (repeatable)"
          ))
    <*> many (OA.strOption
          ( OA.long "warn"
         <> OA.metavar "LINT"
         <> OA.help "Warn on a lint by name (repeatable)"
          ))
    <*> many (OA.strOption
          ( OA.long "deny"
         <> OA.metavar "LINT"
         <> OA.help "Deny a lint by name (repeatable)"
          ))

entryOptionParser :: OA.Parser FilePath
entryOptionParser =
  OA.option
    entryPathReader
    ( OA.long "entry"
   <> OA.metavar "PATH"
   <> OA.help "Build an ad hoc entry path (repeatable)"
    )

positionalEntriesParser :: EntryRequirement -> OA.Parser [FilePath]
positionalEntriesParser = \case
  EntryOptional ->
    many $
      OA.argument
        entryPathReader
        ( OA.metavar "ENTRY..."
       <> OA.help "Positional entry path(s)"
        )
  EntryRequired ->
    some $
      OA.argument
        entryPathReader
        ( OA.metavar "ENTRY..."
       <> OA.help "Positional entry path(s)"
        )

entryPathReader :: OA.ReadM FilePath
entryPathReader =
  OA.eitherReader $ \raw ->
    if looksLikeEntryPath raw
      then Right raw
      else Left "expected a Fuddle entry path (for shorthand mode use something like src/Main.fud)"

looksLikeEntryPath :: FilePath -> Bool
looksLikeEntryPath raw =
     takeExtension raw == ".fud"
  || "/" `isInfixOf` raw
  || "\\" `isInfixOf` raw

openModeParser :: OA.Parser OpenMode
openModeParser =
      OA.flag'
        OpenDefault
        ( OA.long "open"
       <> OA.help "Open the local server in a browser"
        )
  <|> ( OpenPath <$> OA.strOption
        ( OA.long "open-path"
       <> OA.metavar "PATH"
       <> OA.help "Open the local server at a specific path"
        ))
  <|> pure OpenNever

liveReloadParser :: OA.Parser LiveReloadMode
liveReloadParser =
      OA.flag'
        LiveReloadEnabled
        ( OA.long "live-reload"
       <> OA.help "Enable live reload"
        )
  <|> OA.flag'
        LiveReloadDisabled
        ( OA.long "no-live-reload"
       <> OA.help "Disable live reload"
        )
  <|> pure LiveReloadDefault

minifyModeParser :: OA.Parser MinifyMode
minifyModeParser =
      OA.flag'
        MinifyEnabled
        ( OA.long "minify"
       <> OA.help "Force minification"
        )
  <|> OA.flag'
        MinifyDisabled
        ( OA.long "no-minify"
       <> OA.help "Force no minification"
        )
  <|> pure MinifyDefault

includeModeOption :: String -> String -> OA.Parser IncludeMode
includeModeOption longName helpText =
  OA.option
    (enumReader "include mode" includeModeMap)
    ( OA.long longName
   <> OA.metavar "MODE"
   <> OA.value IncludeAuto
   <> OA.help helpText
    )

outputFormatOption :: OutputFormat -> OA.Parser OutputFormat
outputFormatOption defaultValue =
  OA.option
    (enumReader "output format" outputFormatMap)
    ( OA.long "format"
   <> OA.metavar "FORMAT"
   <> OA.value defaultValue
   <> OA.help "Output format"
    )

reportModeOption :: String -> ReportMode -> OA.Parser ReportMode
reportModeOption longName defaultValue =
  OA.option
    (enumReader "report mode" reportModeMap)
    ( OA.long longName
   <> OA.metavar "MODE"
   <> OA.value defaultValue
   <> OA.help "Report mode"
    )

inspectSubjectParser :: OA.Parser InspectSubject
inspectSubjectParser =
  InspectSubject
    <$> optional (OA.strOption
          ( OA.long "target"
         <> OA.metavar "NAME"
         <> OA.help "Inspect a named target"
          ))
    <*> optional (OA.option entryPathReader
          ( OA.long "entry"
         <> OA.metavar "PATH"
         <> OA.help "Inspect an ad hoc entry path"
          ))
    <*> optional (OA.argument
          entryPathReader
          ( OA.metavar "ENTRY"
         <> OA.help "Positional entry path"
          ))
    <*> OA.switch
          ( OA.long "stdin"
         <> OA.help "Read one source from stdin"
          )
    <*> optional (OA.strOption
          ( OA.long "stdin-filepath"
         <> OA.metavar "PATH"
         <> OA.help "Logical file path/module name for stdin source"
          ))
    <*> memberSelectionParser

dumpOptionsParser :: OA.Parser InspectDumpOptions
dumpOptionsParser =
  InspectDumpOptions
    <$> inspectSubjectParser
    <*> outputFormatOption FormatText
    <*> optional (OA.strOption
          ( OA.short 'o'
         <> OA.long "output"
         <> OA.metavar "FILE"
         <> OA.help "Write output to a file"
          ))

--------------------------------------------------------------------------------
-- Command parsers
--------------------------------------------------------------------------------

buildOptionsParser :: EntryRequirement -> OA.Parser BuildOptions
buildOptionsParser entryRequirement =
  BuildOptions
    <$> selectionParser entryRequirement
    <*> optional (OA.strOption
          ( OA.long "profile"
         <> OA.metavar "NAME"
         <> OA.help "Build profile: dev, release, ci, or custom"
          ))
    <*> OA.switch
          ( OA.long "release"
         <> OA.help "Alias for --profile release"
          )
    <*> OA.switch
          ( OA.long "optimize"
         <> OA.help "Enable production-oriented optimization"
          )
    <*> resolutionParser
    <*> lintOptionsParser
    <*> optional (OA.option
          (enumReader "timings mode" timingModeMap)
          ( OA.long "timings"
         <> OA.metavar "MODE"
         <> OA.help "Emit timings data as html or json"
          ))
    <*> optional (OA.strOption
          ( OA.short 'o'
         <> OA.long "output"
         <> OA.metavar "FILE"
         <> OA.help "Write the primary artifact to a single output file"
          ))
    <*> optional (OA.strOption
          ( OA.long "out-dir"
         <> OA.metavar "DIR"
         <> OA.help "Directory for final emitted artifacts"
          ))
    <*> OA.switch
          ( OA.long "stdout"
         <> OA.help "Write the primary artifact to stdout"
          )
    <*> emitItemsParser
    <*> optional (OA.option
          (enumReader "sourcemap mode" sourceMapModeMap)
          ( OA.long "sourcemap"
         <> OA.metavar "MODE"
         <> OA.help "Source map mode: none, inline, external"
          ))
    <*> minifyModeParser
    <*> optional (OA.strOption
          ( OA.long "base"
         <> OA.metavar "PATH"
         <> OA.help "Public asset base path"
          ))
    <*> includeModeOption "with-assets" "Asset handling mode: auto, always, never"
    <*> includeModeOption "with-natives" "Native integration mode: auto, always, never"

checkOptionsParser :: OA.Parser CheckOptions
checkOptionsParser =
  CheckOptions
    <$> selectionParser EntryOptional
    <*> optional (OA.strOption
          ( OA.long "profile"
         <> OA.metavar "NAME"
         <> OA.help "Check profile: normally dev or ci"
          ))
    <*> resolutionParser
    <*> lintOptionsParser
    <*> OA.switch
          ( OA.long "full"
         <> OA.help "Run all compiler phases but do not emit final artifacts"
          )
    <*> OA.switch
          ( OA.long "stdin"
         <> OA.help "Read one source from stdin"
          )
    <*> optional (OA.strOption
          ( OA.long "stdin-filepath"
         <> OA.metavar "PATH"
         <> OA.help "Logical file path/module name for stdin source"
          ))

devOptionsParser :: OA.Parser DevOptions
devOptionsParser =
  DevOptions
    <$> selectionParser EntryOptional
    <*> optional (OA.strOption
          ( OA.long "profile"
         <> OA.metavar "NAME"
         <> OA.help "Development profile"
          ))
    <*> resolutionParser
    <*> optional (OA.strOption
          ( OA.long "host"
         <> OA.metavar "HOST"
         <> OA.help "Host to bind the dev server to"
          ))
    <*> optional (OA.option OA.auto
          ( OA.long "port"
         <> OA.metavar "PORT"
         <> OA.help "Port to bind the dev server to"
          ))
    <*> OA.switch
          ( OA.long "strict-port"
         <> OA.help "Fail instead of searching for another port"
          )
    <*> openModeParser
    <*> optional (OA.option
          (boolReader "clear-screen")
          ( OA.long "clear-screen"
         <> OA.metavar "BOOL"
         <> OA.help "Whether to clear the terminal between rebuilds"
          ))
    <*> optional (OA.option OA.auto
          ( OA.long "poll"
         <> OA.metavar "MS"
         <> OA.help "Use polling file watching with the given interval"
          ))
    <*> optional (OA.option OA.auto
          ( OA.long "debounce"
         <> OA.metavar "MS"
         <> OA.help "Debounce file system events"
          ))
    <*> liveReloadParser
    <*> optional (OA.strOption
          ( OA.long "proxy"
         <> OA.metavar "URL"
         <> OA.help "Proxy unmatched requests to an upstream backend"
          ))
    <*> optional (OA.strOption
          ( OA.long "easywordy-root"
         <> OA.metavar "DIR"
         <> OA.help "Explicit EasyWordy installation root"
          ))
    <*> optional (OA.strOption
          ( OA.long "easywordy-url"
         <> OA.metavar "URL"
         <> OA.help "Explicit EasyWordy URL for integration flows"
          ))
    <*> includeModeOption "with-assets" "Asset handling mode: auto, always, never"
    <*> includeModeOption "with-natives" "Native integration mode: auto, always, never"
    <*> OA.switch
          ( OA.long "full-rebuild"
         <> OA.help "Ignore incremental cache for the first cycle"
          )
    <*> optional (OA.strOption
          ( OA.long "base"
         <> OA.metavar "PATH"
         <> OA.help "Public asset base path"
          ))

previewOptionsParser :: OA.Parser PreviewOptions
previewOptionsParser =
  PreviewOptions
    <$> selectionParser EntryOptional
    <*> optional (OA.strOption
          ( OA.long "host"
         <> OA.metavar "HOST"
         <> OA.help "Host to bind the preview server to"
          ))
    <*> optional (OA.option OA.auto
          ( OA.long "port"
         <> OA.metavar "PORT"
         <> OA.help "Port to bind the preview server to"
          ))
    <*> OA.switch
          ( OA.long "strict-port"
         <> OA.help "Fail instead of searching for another port"
          )
    <*> openModeParser
    <*> optional (OA.strOption
          ( OA.long "out-dir"
         <> OA.metavar "DIR"
         <> OA.help "Serve an explicit output directory"
          ))

testOptionsParser :: OA.Parser TestOptions
testOptionsParser =
  TestOptions
    <$> selectionParser EntryOptional
    <*> optional (OA.strOption
          ( OA.long "profile"
         <> OA.metavar "NAME"
         <> OA.help "Test profile; ci is typical"
          ))
    <*> resolutionParser
    <*> optional (OA.strOption
          ( OA.long "filter"
         <> OA.metavar "EXPR"
         <> OA.help "Run only tests matching the expression"
          ))
    <*> OA.switch
          ( OA.long "watch"
         <> OA.help "Watch source changes and re-run tests"
          )
    <*> OA.switch
          ( OA.long "no-run"
         <> OA.help "Compile tests only"
          )
    <*> optional (OA.option
          (enumReader "coverage mode" coverageModeMap)
          ( OA.long "coverage"
         <> OA.metavar "MODE"
         <> OA.help "Coverage mode: off, text, json, lcov"
          ))
    <*> OA.switch
          ( OA.long "update-snapshots"
         <> OA.help "Update snapshot baselines"
          )
    <*> reportModeOption "report" ReportHuman

fmtOptionsParser :: OA.Parser FmtOptions
fmtOptionsParser =
  FmtOptions
    <$> many (OA.strArgument
          ( OA.metavar "FILES..."
         <> OA.help "Files to format"
          ))
    <*> OA.switch
          ( OA.long "check"
         <> OA.help "Exit non-zero if formatting would change files"
          )
    <*> OA.switch
          ( OA.long "write"
         <> OA.help "Rewrite files in place"
          )
    <*> OA.switch
          ( OA.long "stdin"
         <> OA.help "Read one source from stdin"
          )
    <*> optional (OA.strOption
          ( OA.long "stdin-filepath"
         <> OA.metavar "PATH"
         <> OA.help "Logical file path/module name for stdin source"
          ))
    <*> reportModeOption "report" ReportHuman

replOptionsParser :: OA.Parser ReplOptions
replOptionsParser =
  ReplOptions
    <$> optional (OA.strOption
          ( OA.long "preload"
         <> OA.metavar "TARGET"
         <> OA.help "Preload a target or entry before starting the REPL"
          ))
    <*> many (OA.strOption
          ( OA.long "import"
         <> OA.metavar "MODULE"
         <> OA.help "Import a module before starting the REPL"
          ))
    <*> optional (OA.option
          (enumReader "runtime target" runtimeTargetMap)
          ( OA.long "target"
         <> OA.metavar "TARGET"
         <> OA.help "Runtime target: browser or ssr"
          ))
    <*> reportModeOption "report" ReportHuman

cleanOptionsParser :: OA.Parser CleanOptions
cleanOptionsParser =
  CleanOptions
    <$> OA.switch
          ( OA.long "artifacts"
         <> OA.help "Remove build artifacts"
          )
    <*> OA.switch
          ( OA.long "cache"
         <> OA.help "Remove compiler and incremental caches"
          )
    <*> OA.switch
          ( OA.long "deps"
         <> OA.help "Remove local dependency materialization"
          )
    <*> OA.switch
          ( OA.long "natives"
         <> OA.help "Remove native build outputs"
          )
    <*> OA.switch
          ( OA.long "all"
         <> OA.help "Remove everything known to the compiler toolchain"
          )

initOptionsParser :: OA.Parser InitOptions
initOptionsParser =
  InitOptions
    <$> OA.option
          (enumReader "scaffold kind" scaffoldKindMap)
          ( OA.long "kind"
         <> OA.metavar "KIND"
         <> OA.value ScaffoldProject
         <> OA.help "Scaffold kind: project, workspace, wapp"
          )
    <*> OA.switch
          ( OA.long "force"
         <> OA.help "Overwrite or initialize in a non-empty directory"
          )

newOptionsParser :: OA.Parser NewOptions
newOptionsParser =
  NewOptions
    <$> OA.strArgument
          ( OA.metavar "PATH"
         <> OA.help "Directory to create"
          )
    <*> OA.option
          (enumReader "scaffold kind" scaffoldKindMap)
          ( OA.long "kind"
         <> OA.metavar "KIND"
         <> OA.value ScaffoldProject
         <> OA.help "Scaffold kind: project, workspace, wapp"
          )
    <*> optional (OA.strOption
          ( OA.long "template"
         <> OA.metavar "NAME"
         <> OA.help "Template name"
          ))
    <*> OA.switch
          ( OA.long "force"
         <> OA.help "Overwrite an existing path"
          )

helpOptionsParser :: OA.Parser HelpOptions
helpOptionsParser =
  HelpOptions
    <$> many (OA.strArgument
          ( OA.metavar "TOPIC..."
         <> OA.help "Optional command/topic path"
          ))

metadataOptionsParser :: OA.Parser MetadataOptions
metadataOptionsParser =
  MetadataOptions
    <$> memberSelectionParser
    <*> OA.option
          (enumReader "metadata format" metadataFormatMap)
          ( OA.long "format"
         <> OA.metavar "FORMAT"
         <> OA.value MetadataJson
         <> OA.help "Metadata format"
          )
    <*> optional (OA.option OA.auto
          ( OA.long "format-version"
         <> OA.metavar "N"
         <> OA.help "Explicit metadata schema version"
          ))
    <*> OA.switch
          ( OA.long "no-deps"
         <> OA.help "Do not include dependency details"
          )
    <*> OA.switch
          ( OA.long "targets-only"
         <> OA.help "Only include target descriptions"
          )

doctorOptionsParser :: OA.Parser DoctorOptions
doctorOptionsParser =
  DoctorOptions
    <$> OA.switch
          ( OA.long "ci"
         <> OA.help "Run in CI-oriented mode"
          )
    <*> OA.switch
          ( OA.long "fix"
         <> OA.help "Apply safe automatic fixes where possible"
          )
    <*> optional (OA.strOption
          ( OA.long "easywordy-root"
         <> OA.metavar "DIR"
         <> OA.help "Explicit EasyWordy installation root"
          ))

explainOptionsParser :: OA.Parser ExplainOptions
explainOptionsParser =
  ExplainOptions
    <$> OA.strArgument
          ( OA.metavar "CODE"
         <> OA.help "Diagnostic code"
          )

completionsOptionsParser :: OA.Parser CompletionsOptions
completionsOptionsParser =
  CompletionsOptions
    <$> OA.argument
          (enumReader "shell" shellMap)
          ( OA.metavar "SHELL"
         <> OA.help "Target shell"
          )

--------------------------------------------------------------------------------
-- Dependency parser tree
--------------------------------------------------------------------------------

depsCommandParser :: OA.Parser DepsCommand
depsCommandParser =
  OA.hsubparser $
       command' "fetch" (DepsFetchCmd <$> depsFetchOptionsParser) "Prefetch dependencies for offline or CI builds"
    <> command' "lock" (DepsLockCmd <$> depsLockOptionsParser) "Generate, validate, or refresh the lockfile"
    <> command' "tree" (DepsTreeCmd <$> depsTreeOptionsParser) "Render the dependency tree"
    <> command' "vendor" (DepsVendorCmd <$> depsVendorOptionsParser) "Vendor dependencies locally"
    <> command' "why" (DepsWhyCmd <$> depsWhyOptionsParser) "Explain why a package is present"
  where
    command' name parser desc =
      OA.command name (OA.info parser (OA.progDesc desc))

depsFetchOptionsParser :: OA.Parser DepsFetchOptions
depsFetchOptionsParser =
  DepsFetchOptions
    <$> memberSelectionParser
    <*> optional (OA.strOption
          ( OA.long "target-platform"
         <> OA.metavar "TRIPLE"
         <> OA.help "Target platform triple"
          ))
    <*> OA.switch
          ( OA.long "all-platforms"
         <> OA.help "Fetch for all declared platforms"
          )
    <*> OA.switch
          ( OA.long "locked"
         <> OA.help "Require an up-to-date lockfile"
          )
    <*> OA.switch
          ( OA.long "offline"
         <> OA.help "Do not access the network"
          )

depsLockOptionsParser :: OA.Parser DepsLockOptions
depsLockOptionsParser =
  DepsLockOptions
    <$> memberSelectionParser
    <*> OA.switch
          ( OA.long "check"
         <> OA.help "Fail if the lockfile would change"
          )
    <*> OA.switch
          ( OA.long "update"
         <> OA.help "Refresh the lockfile"
          )

depsTreeOptionsParser :: OA.Parser DepsTreeOptions
depsTreeOptionsParser =
  DepsTreeOptions
    <$> memberSelectionParser
    <*> outputFormatOption FormatText
    <*> OA.switch
          ( OA.long "duplicates"
         <> OA.help "Highlight duplicated dependencies"
          )
    <*> optional (OA.strOption
          ( OA.long "invert"
         <> OA.metavar "PACKAGE"
         <> OA.help "Invert the tree around a specific package"
          ))

depsVendorOptionsParser :: OA.Parser DepsVendorOptions
depsVendorOptionsParser =
  DepsVendorOptions
    <$> memberSelectionParser
    <*> optional (OA.strOption
          ( OA.long "out-dir"
         <> OA.metavar "DIR"
         <> OA.help "Destination directory for vendored dependencies"
          ))
    <*> OA.switch
          ( OA.long "symlink"
         <> OA.help "Create symlinks instead of copying"
          )

depsWhyOptionsParser :: OA.Parser DepsWhyOptions
depsWhyOptionsParser =
  DepsWhyOptions
    <$> memberSelectionParser
    <*> OA.strArgument
          ( OA.metavar "PACKAGE"
         <> OA.help "Package to explain"
          )

--------------------------------------------------------------------------------
-- Inspect parser tree
--------------------------------------------------------------------------------

inspectCommandParser :: OA.Parser InspectCommand
inspectCommandParser =
  OA.hsubparser $
       command' "cst" (InspectCstCmd <$> dumpOptionsParser) "Inspect the concrete syntax tree"
    <> command' "ast" (InspectAstCmd <$> dumpOptionsParser) "Inspect the analyzed AST"
    <> command' "symbols" (InspectSymbolsCmd <$> dumpOptionsParser) "Inspect the symbol table and bindings"
    <> command' "graph" (InspectGraphCmd <$> inspectGraphOptionsParser) "Inspect the resolved module graph"
    <> command' "ir" (InspectIrCmd <$> inspectIrOptionsParser) "Inspect a specific IR lowering stage"
    <> command' "js" (InspectJsCmd <$> dumpOptionsParser) "Inspect the JS printer or final JS-facing form"
  where
    command' name parser desc =
      OA.command name (OA.info parser (OA.progDesc desc))

inspectGraphAliasParser :: OA.Parser InspectCommand
inspectGraphAliasParser =
  InspectGraphCmd <$> inspectGraphOptionsParser

inspectIrAliasParser :: OA.Parser InspectCommand
inspectIrAliasParser =
  InspectIrCmd <$> inspectIrOptionsParser

inspectGraphOptionsParser :: OA.Parser InspectGraphOptions
inspectGraphOptionsParser =
  InspectGraphOptions
    <$> memberSelectionParser
    <*> outputFormatOption FormatText
    <*> optional (OA.strOption
          ( OA.short 'o'
         <> OA.long "output"
         <> OA.metavar "FILE"
         <> OA.help "Write output to a file"
          ))

inspectIrOptionsParser :: OA.Parser InspectIrOptions
inspectIrOptionsParser =
  InspectIrOptions
    <$> inspectSubjectParser
    <*> OA.strOption
          ( OA.long "stage"
         <> OA.metavar "NAME"
         <> OA.help "IR stage name"
          )
    <*> outputFormatOption FormatText
    <*> optional (OA.strOption
          ( OA.short 'o'
         <> OA.long "output"
         <> OA.metavar "FILE"
         <> OA.help "Write output to a file"
          ))

--------------------------------------------------------------------------------
-- Wapp parser tree
--------------------------------------------------------------------------------

wappCommandParser :: OA.Parser WappCommand
wappCommandParser =
  OA.hsubparser $
       command' "stage" (WappStageCmd <$> wappStageOptionsParser) "Produce a canonical staged Wapp tree"
    <> command' "install" (WappInstallCmd <$> wappInstallOptionsParser) "Install a staged Wapp into EasyWordy"
    <> command' "layout" (WappLayoutCmd <$> wappLayoutOptionsParser) "Show the expected Wapp layout and install destinations"
  where
    command' name parser desc =
      OA.command name (OA.info parser (OA.progDesc desc))

wappStageOptionsParser :: OA.Parser WappStageOptions
wappStageOptionsParser =
  WappStageOptions
    <$> optional (OA.strOption
          ( OA.long "profile"
         <> OA.metavar "NAME"
         <> OA.help "Profile to stage"
          ))
    <*> optional (OA.strOption
          ( OA.long "out-dir"
         <> OA.metavar "DIR"
         <> OA.help "Explicit staging directory"
          ))
    <*> includeModeOption "with-assets" "Asset staging mode: auto, always, never"
    <*> includeModeOption "with-natives" "Native staging mode: auto, always, never"
    <*> OA.switch
          ( OA.long "copy-assets"
         <> OA.help "Copy assets into the staged tree"
          )
    <*> OA.switch
          ( OA.long "link-assets"
         <> OA.help "Symlink assets into the staged tree"
          )

wappInstallOptionsParser :: OA.Parser WappInstallOptions
wappInstallOptionsParser =
  WappInstallOptions
    <$> optional (OA.strOption
          ( OA.long "easywordy-root"
         <> OA.metavar "DIR"
         <> OA.help "Explicit EasyWordy installation root"
          ))
    <*> optional (OA.strOption
          ( OA.long "profile"
         <> OA.metavar "NAME"
         <> OA.help "Profile to install"
          ))
    <*> optional (OA.option
          (enumReader "install mode" installModeMap)
          ( OA.long "install-mode"
         <> OA.metavar "MODE"
         <> OA.help "Install mode: copy or link"
          ))
    <*> includeModeOption "with-assets" "Asset installation mode: auto, always, never"
    <*> includeModeOption "with-natives" "Native installation mode: auto, always, never"
    <*> optional (OA.option
          (enumReader "restart mode" restartModeMap)
          ( OA.long "restart"
         <> OA.metavar "MODE"
         <> OA.help "Restart mode: none, touch, command"
          ))
    <*> OA.switch
          ( OA.long "dry-run"
         <> OA.help "Show what would be installed without changing anything"
          )

wappLayoutOptionsParser :: OA.Parser WappLayoutOptions
wappLayoutOptionsParser =
  WappLayoutOptions
    <$> outputFormatOption FormatText

--------------------------------------------------------------------------------
-- Native parser tree
--------------------------------------------------------------------------------

nativeCommandParser :: OA.Parser NativeCommand
nativeCommandParser =
  OA.hsubparser $
       command' "build" (NativeBuildCmd <$> nativeBuildOptionsParser) "Build native integrations"
    <> command' "install" (NativeInstallCmd <$> nativeInstallOptionsParser) "Install native integrations into EasyWordy"
    <> command' "clean" (pure NativeCleanCmd) "Remove native build outputs"
    <> command' "doctor" (pure NativeDoctorCmd) "Validate the native toolchain"
  where
    command' name parser desc =
      OA.command name (OA.info parser (OA.progDesc desc))

nativeBuildOptionsParser :: OA.Parser NativeBuildOptions
nativeBuildOptionsParser =
  NativeBuildOptions
    <$> optional (OA.strOption
          ( OA.long "profile"
         <> OA.metavar "NAME"
         <> OA.help "Native build profile"
          ))
    <*> optional (OA.option
          (enumReader "native tool" toolChoiceMap)
          ( OA.long "tool"
         <> OA.metavar "TOOL"
         <> OA.help "Native build tool: auto, stack, cabal"
          ))
    <*> OA.switch
          ( OA.long "watch"
         <> OA.help "Watch native sources and rebuild"
          )
    <*> optional (OA.option OA.auto
          ( OA.long "jobs"
         <> OA.metavar "N"
         <> OA.help "Maximum number of concurrent native jobs"
          ))
    <*> optional (OA.strOption
          ( OA.long "out-dir"
         <> OA.metavar "DIR"
         <> OA.help "Directory for native outputs"
          ))
    <*> optional (OA.strOption
          ( OA.long "target-platform"
         <> OA.metavar "TRIPLE"
         <> OA.help "Target platform triple"
          ))
    <*> many (OA.strArgument
          ( OA.metavar "TOOL-ARGS..."
         <> OA.help "Arguments forwarded to the native tool (put them after --)"
          ))

nativeInstallOptionsParser :: OA.Parser NativeInstallOptions
nativeInstallOptionsParser =
  NativeInstallOptions
    <$> optional (OA.strOption
          ( OA.long "easywordy-root"
         <> OA.metavar "DIR"
         <> OA.help "Explicit EasyWordy installation root"
          ))
    <*> optional (OA.option
          (enumReader "install mode" installModeMap)
          ( OA.long "install-mode"
         <> OA.metavar "MODE"
         <> OA.help "Install mode: copy or link"
          ))

--------------------------------------------------------------------------------
-- Enum readers
--------------------------------------------------------------------------------

enumReader :: String -> [(String, a)] -> OA.ReadM a
enumReader label pairs =
  OA.eitherReader $ \raw ->
    case lookup (normalizeKey raw) normalizedPairs of
      Just value ->
        Right value
      Nothing ->
        Left $
          "invalid " <> label <> " "
            <> show raw
            <> " (expected one of: "
            <> intercalate ", " (map fst normalizedPairs)
            <> ")"
  where
  normalizedPairs = fmap (\(key, value) -> (normalizeKey key, value)) pairs

boolReader :: String -> OA.ReadM Bool
boolReader label =
  OA.eitherReader $ \raw ->
    case normalizeKey raw of
      "true" -> Right True
      "false" -> Right False
      "yes" -> Right True
      "no" -> Right False
      "1" -> Right True
      "0" -> Right False
      _ ->
        Left $
          "invalid " <> label <> " "
            <> show raw
            <> " (expected true/false)"

emitItemsParser :: OA.Parser [EmitItem]
emitItemsParser =
  concat <$> many
    (OA.option
      emitItemListReader
      ( OA.long "emit"
     <> OA.metavar "ITEMS"
     <> OA.help "Emit items; repeatable and comma-separated (e.g. --emit js,css --emit ir:TypeIrMod)"
      ))

emitItemListReader :: OA.ReadM [EmitItem]
emitItemListReader =
  OA.eitherReader $ \raw ->
    traverse parseEmitItem (splitComma raw)

parseEmitItem :: String -> Either String EmitItem
parseEmitItem raw =
  case normalizeKey raw of
    "js" -> Right EmitJs
    "js-map" -> Right EmitJsMap
    "css" -> Right EmitCss
    "asset-manifest" -> Right EmitAssetManifest
    "wapp-layout" -> Right EmitWappLayout
    "meta" -> Right EmitMeta
    "cst" -> Right EmitCst
    "ast" -> Right EmitAst
    "symbols" -> Right EmitSymbols
    "module-graph" -> Right EmitModuleGraph
    "js-ir" -> Right EmitJsIr
    _ ->
      case stripPrefix "ir:" raw of
        Just stageName | not (null (trim stageName)) ->
          Right (EmitIr (trim stageName))
        _ ->
          Left $
            "invalid emit item "
              <> show raw
              <> " (expected js, js-map, css, asset-manifest, wapp-layout, meta, cst, ast, symbols, module-graph, js-ir, or ir:<stage>)"

colorModeMap :: [(String, ColorMode)]
colorModeMap =
  [ ("auto", ColorAuto)
  , ("always", ColorAlways)
  , ("never", ColorNever)
  ]

reportModeMap :: [(String, ReportMode)]
reportModeMap =
  [ ("human", ReportHuman)
  , ("json", ReportJson)
  ]

outputFormatMap :: [(String, OutputFormat)]
outputFormatMap =
  [ ("text", FormatText)
  , ("json", FormatJson)
  , ("dot", FormatDot)
  , ("mermaid", FormatMermaid)
  , ("html", FormatHtml)
  ]

metadataFormatMap :: [(String, MetadataFormat)]
metadataFormatMap =
  [ ("json", MetadataJson)
  ]

timingModeMap :: [(String, TimingMode)]
timingModeMap =
  [ ("html", TimingHtml)
  , ("json", TimingJson)
  ]

coverageModeMap :: [(String, CoverageMode)]
coverageModeMap =
  [ ("off", CoverageOff)
  , ("text", CoverageText)
  , ("json", CoverageJson)
  , ("lcov", CoverageLcov)
  ]

sourceMapModeMap :: [(String, SourceMapMode)]
sourceMapModeMap =
  [ ("none", SourceMapNone)
  , ("inline", SourceMapInline)
  , ("external", SourceMapExternal)
  ]

includeModeMap :: [(String, IncludeMode)]
includeModeMap =
  [ ("auto", IncludeAuto)
  , ("always", IncludeAlways)
  , ("never", IncludeNever)
  ]

runtimeTargetMap :: [(String, RuntimeTarget)]
runtimeTargetMap =
  [ ("browser", RuntimeBrowser)
  , ("ssr", RuntimeSsr)
  ]

scaffoldKindMap :: [(String, ScaffoldKind)]
scaffoldKindMap =
  [ ("project", ScaffoldProject)
  , ("workspace", ScaffoldWorkspace)
  , ("wapp", ScaffoldWapp)
  ]

installModeMap :: [(String, InstallMode)]
installModeMap =
  [ ("copy", InstallCopy)
  , ("link", InstallLink)
  ]

restartModeMap :: [(String, RestartMode)]
restartModeMap =
  [ ("none", RestartNone)
  , ("touch", RestartTouch)
  , ("command", RestartCommand)
  ]

toolChoiceMap :: [(String, ToolChoice)]
toolChoiceMap =
  [ ("auto", ToolAuto)
  , ("stack", ToolStack)
  , ("cabal", ToolCabal)
  ]

shellMap :: [(String, Shell)]
shellMap =
  [ ("bash", ShellBash)
  , ("zsh", ShellZsh)
  , ("fish", ShellFish)
  , ("powershell", ShellPowerShell)
  , ("elvish", ShellElvish)
  ]

--------------------------------------------------------------------------------
-- Response-file expansion
--------------------------------------------------------------------------------

expandResponseFilesIO :: [String] -> IO [String]
expandResponseFilesIO =
  fmap concat . traverse (expandArg "." Set.empty)

expandArg :: FilePath -> Set.Set FilePath -> String -> IO [String]
expandArg baseDir seen raw =
  case stripPrefix "@" raw of
    Nothing ->
      pure [raw]
    Just relOrAbs -> do
      let path =
            normalise $
              if isRelative relOrAbs
                then baseDir </> relOrAbs
                else relOrAbs
      exists <- doesFileExist path
      if not exists
        then pure [raw]
        else
          if Set.member path seen
            then
              ioError $
                userError $
                  "response-file cycle detected while expanding " <> show path
            else do
              content <- readFile path
              tokens <- either
                (\err -> ioError (userError ("failed to parse response file " <> show path <> ": " <> err)))
                pure
                (tokenizeResponseFile content)

              fmap concat $
                traverse
                  (expandArg (takeDirectory path) (Set.insert path seen))
                  tokens


tokenizeResponseFile :: String -> Either String [String]
tokenizeResponseFile =
  finish <=< iterState [] [] Normal
  where
  finish :: ([String], String, TokenState) -> Either String [String]
  finish (tokensAcc, currentAcc, state) =
    case state of
      Normal ->
        Right (reverse (flushCurrent tokensAcc currentAcc))
      InSingle ->
        Left "unterminated single-quoted string"
      InDouble ->
        Left "unterminated double-quoted string"
      EscapeNormal ->
        Left "dangling escape at end of file"
      EscapeDouble ->
        Left "dangling escape inside double-quoted string"

  iterState tokensAcc currentAcc state input =
    case input of
      [] ->
        Right (tokensAcc, currentAcc, state)

      c : cs ->
        case state of
          Normal
            | isSpace c ->
                iterState (flushCurrent tokensAcc currentAcc) [] Normal cs
            | c == '#' && null currentAcc ->
                iterState tokensAcc currentAcc Normal (dropWhile (/= '\n') cs)
            | c == '\'' ->
                iterState tokensAcc currentAcc InSingle cs
            | c == '"' ->
                iterState tokensAcc currentAcc InDouble cs
            | c == '\\' ->
                iterState tokensAcc currentAcc EscapeNormal cs
            | otherwise ->
                iterState tokensAcc (c : currentAcc) Normal cs

          InSingle
            | c == '\'' ->
                iterState tokensAcc currentAcc Normal cs
            | otherwise ->
                iterState tokensAcc (c : currentAcc) InSingle cs

          InDouble
            | c == '"' ->
                iterState tokensAcc currentAcc Normal cs
            | c == '\\' ->
                iterState tokensAcc currentAcc EscapeDouble cs
            | otherwise ->
                iterState tokensAcc (c : currentAcc) InDouble cs

          EscapeNormal ->
            iterState tokensAcc (c : currentAcc) Normal cs

          EscapeDouble ->
            iterState tokensAcc (decodeDoubleEscape c : currentAcc) InDouble cs

  flushCurrent tokensAcc currentAcc =
    case reverse currentAcc of
      [] -> tokensAcc
      token -> token : tokensAcc

  decodeDoubleEscape = \case
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    '"' -> '"'
    '\\' -> '\\'
    other -> other

--------------------------------------------------------------------------------
-- Small helpers
--------------------------------------------------------------------------------

normalizeKey :: String -> String
normalizeKey =
  fmap toLower . trim

trim :: String -> String
trim =
  dropWhileEndSpace . dropWhile isSpace
  where
    dropWhileEndSpace = reverse . dropWhile isSpace . reverse

splitComma :: String -> [String]
splitComma raw =
  case iterState raw [] [] of
    [] -> []
    xs -> xs
  where
    iterState input chunkAcc tokenAcc =
      case input of
        [] ->
          let token = trim (reverse chunkAcc)
          in if null token then reverse tokenAcc else reverse (token : tokenAcc)

        ',' : cs ->
          let token = trim (reverse chunkAcc)
          in if null token
               then iterState cs [] tokenAcc
               else iterState cs [] (token : tokenAcc)

        c : cs ->
          iterState cs (c : chunkAcc) tokenAcc

(<=<) :: (b -> Either e c) -> (a -> Either e b) -> a -> Either e c
(<=<) f g x =
  g x >>= f