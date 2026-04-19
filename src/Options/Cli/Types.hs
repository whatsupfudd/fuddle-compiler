module Options.Cli.Types where

data Cli = Cli
  { globals :: GlobalOptions
  , command :: Command
  }
  deriving (Eq, Show)

data GlobalOptions = GlobalOptions
  { workingDir :: Maybe FilePath
  , manifestPath :: Maybe FilePath
  , verbosity :: Int
  , quiet :: Bool
  , color :: ColorMode
  , report :: ReportMode
  , configFiles :: [FilePath]
  , setValues :: [String]
  , unstable :: [String]
  , noProgress :: Bool
  }
  deriving (Eq, Show)

data Command
  = BuildCmd BuildOptions
  | CheckCmd CheckOptions
  | DevCmd DevOptions
  | PreviewCmd PreviewOptions
  | TestCmd TestOptions
  | FmtCmd FmtOptions
  | ReplCmd ReplOptions
  | CleanCmd CleanOptions
  | InitCmd InitOptions
  | NewCmd NewOptions
  | DepsCmd DepsCommand
  | MetadataCmd MetadataOptions
  | InspectCmd InspectCommand
  | DoctorCmd DoctorOptions
  | ExplainCmd ExplainOptions
  | CompletionsCmd CompletionsOptions
  | HelpCmd HelpOptions
  | VersionCmd
  | WappCmd WappCommand
  | NativeCmd NativeCommand
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Shared option structures
--------------------------------------------------------------------------------

data MemberSelection = MemberSelection
  { workspace :: Bool
  , packages :: [String]
  }
  deriving (Eq, Show)

data Selection = Selection
  { targets :: [String]
  , allTargets :: Bool
  , entries :: [FilePath]
  , members :: MemberSelection
  }
  deriving (Eq, Show)

data ResolutionOptions = ResolutionOptions
  { jobs :: Maybe Int
  , keepGoing :: Bool
  , offline :: Bool
  , locked :: Bool
  , frozen :: Bool
  , targetDir :: Maybe FilePath
  , cacheDir :: Maybe FilePath
  }
  deriving (Eq, Show)

data LintOptions = LintOptions
  { warningsAsErrors :: Bool
  , allow :: [String]
  , warn :: [String]
  , deny :: [String]
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Main command payloads
--------------------------------------------------------------------------------

data BuildOptions = BuildOptions
  { selection :: Selection
  , profile :: Maybe String
  , release :: Bool
  , optimize :: Bool
  , resolution :: ResolutionOptions
  , lints :: LintOptions
  , timings :: Maybe TimingMode
  , output :: Maybe FilePath
  , outDir :: Maybe FilePath
  , stdout :: Bool
  , emit :: [EmitItem]
  , sourceMap :: Maybe SourceMapMode
  , minify :: MinifyMode
  , basePath :: Maybe FilePath
  , withAssets :: IncludeMode
  , withNatives :: IncludeMode
  }
  deriving (Eq, Show)

data CheckOptions = CheckOptions
  { selection :: Selection
  , profile :: Maybe String
  , resolution :: ResolutionOptions
  , lints :: LintOptions
  , full :: Bool
  , stdin :: Bool
  , stdinFilepath :: Maybe FilePath
  }
  deriving (Eq, Show)

data DevOptions = DevOptions
  { selection :: Selection
  , profile :: Maybe String
  , resolution :: ResolutionOptions
  , host :: Maybe String
  , port :: Maybe Int
  , strictPort :: Bool
  , open :: OpenMode
  , clearScreen :: Maybe Bool
  , pollMs :: Maybe Int
  , debounceMs :: Maybe Int
  , liveReload :: LiveReloadMode
  , proxyUrl :: Maybe String
  , easywordyRoot :: Maybe FilePath
  , easywordyUrl :: Maybe String
  , withAssets :: IncludeMode
  , withNatives :: IncludeMode
  , fullRebuild :: Bool
  , basePath :: Maybe FilePath
  }
  deriving (Eq, Show)

data PreviewOptions = PreviewOptions
  { selection :: Selection
  , host :: Maybe String
  , port :: Maybe Int
  , strictPort :: Bool
  , open :: OpenMode
  , outDir :: Maybe FilePath
  }
  deriving (Eq, Show)

data TestOptions = TestOptions
  { selection :: Selection
  , profile :: Maybe String
  , resolution :: ResolutionOptions
  , filterExpr :: Maybe String
  , watch :: Bool
  , noRun :: Bool
  , coverage :: Maybe CoverageMode
  , updateSnapshots :: Bool
  , report :: ReportMode
  }
  deriving (Eq, Show)

data FmtOptions = FmtOptions
  { files :: [FilePath]
  , check :: Bool
  , write :: Bool
  , stdin :: Bool
  , stdinFilepath :: Maybe FilePath
  , report :: ReportMode
  }
  deriving (Eq, Show)

data ReplOptions = ReplOptions
  { preload :: Maybe String
  , imports :: [String]
  , runtimeTarget :: Maybe RuntimeTarget
  , report :: ReportMode
  }
  deriving (Eq, Show)

data CleanOptions = CleanOptions
  { artifacts :: Bool
  , cache :: Bool
  , deps :: Bool
  , natives :: Bool
  , allClean :: Bool
  }
  deriving (Eq, Show)

data InitOptions = InitOptions
  { kind :: ScaffoldKind
  , force :: Bool
  }
  deriving (Eq, Show)

data NewOptions = NewOptions
  { path :: FilePath
  , kind :: ScaffoldKind
  , template :: Maybe String
  , force :: Bool
  }
  deriving (Eq, Show)

data HelpOptions = HelpOptions
  { topics :: [String]
  }
  deriving (Eq, Show)

data MetadataOptions = MetadataOptions
  { members :: MemberSelection
  , format :: MetadataFormat
  , formatVersion :: Maybe Int
  , noDeps :: Bool
  , targetsOnly :: Bool
  }
  deriving (Eq, Show)

data DoctorOptions = DoctorOptions
  { ci :: Bool
  , fix :: Bool
  , easywordyRoot :: Maybe FilePath
  }
  deriving (Eq, Show)

data ExplainOptions = ExplainOptions
  { code :: String
  }
  deriving (Eq, Show)

data CompletionsOptions = CompletionsOptions
  { shell :: Shell
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Dependency commands
--------------------------------------------------------------------------------

data DepsCommand
  = DepsFetchCmd DepsFetchOptions
  | DepsLockCmd DepsLockOptions
  | DepsTreeCmd DepsTreeOptions
  | DepsVendorCmd DepsVendorOptions
  | DepsWhyCmd DepsWhyOptions
  deriving (Eq, Show)

data DepsFetchOptions = DepsFetchOptions
  { members :: MemberSelection
  , targetPlatform :: Maybe String
  , allPlatforms :: Bool
  , locked :: Bool
  , offline :: Bool
  }
  deriving (Eq, Show)

data DepsLockOptions = DepsLockOptions
  { members :: MemberSelection
  , check :: Bool
  , update :: Bool
  }
  deriving (Eq, Show)

data DepsTreeOptions = DepsTreeOptions
  { members :: MemberSelection
  , format :: OutputFormat
  , duplicates :: Bool
  , invert :: Maybe String
  }
  deriving (Eq, Show)

data DepsVendorOptions = DepsVendorOptions
  { members :: MemberSelection
  , outDir :: Maybe FilePath
  , symlink :: Bool
  }
  deriving (Eq, Show)

data DepsWhyOptions = DepsWhyOptions
  { members :: MemberSelection
  , package :: String
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Inspect commands
--------------------------------------------------------------------------------

data InspectCommand
  = InspectCstCmd InspectDumpOptions
  | InspectAstCmd InspectDumpOptions
  | InspectSymbolsCmd InspectDumpOptions
  | InspectGraphCmd InspectGraphOptions
  | InspectIrCmd InspectIrOptions
  | InspectJsCmd InspectDumpOptions
  deriving (Eq, Show)

data InspectSubject = InspectSubject
  { target :: Maybe String
  , entry :: Maybe FilePath
  , positional :: Maybe FilePath
  , stdin :: Bool
  , stdinFilepath :: Maybe FilePath
  , members :: MemberSelection
  }
  deriving (Eq, Show)

data InspectDumpOptions = InspectDumpOptions
  { subject :: InspectSubject
  , format :: OutputFormat
  , output :: Maybe FilePath
  }
  deriving (Eq, Show)

data InspectGraphOptions = InspectGraphOptions
  { members :: MemberSelection
  , format :: OutputFormat
  , output :: Maybe FilePath
  }
  deriving (Eq, Show)

data InspectIrOptions = InspectIrOptions
  { subject :: InspectSubject
  , stage :: String
  , format :: OutputFormat
  , output :: Maybe FilePath
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- EasyWordy / Wapp commands
--------------------------------------------------------------------------------

data WappCommand
  = WappStageCmd WappStageOptions
  | WappInstallCmd WappInstallOptions
  | WappLayoutCmd WappLayoutOptions
  deriving (Eq, Show)

data WappStageOptions = WappStageOptions
  { profile :: Maybe String
  , outDir :: Maybe FilePath
  , withAssets :: IncludeMode
  , withNatives :: IncludeMode
  , copyAssets :: Bool
  , linkAssets :: Bool
  }
  deriving (Eq, Show)

data WappInstallOptions = WappInstallOptions
  { easywordyRoot :: Maybe FilePath
  , profile :: Maybe String
  , installMode :: Maybe InstallMode
  , withAssets :: IncludeMode
  , withNatives :: IncludeMode
  , restart :: Maybe RestartMode
  , dryRun :: Bool
  }
  deriving (Eq, Show)

data WappLayoutOptions = WappLayoutOptions
  { format :: OutputFormat
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Native commands
--------------------------------------------------------------------------------

data NativeCommand
  = NativeBuildCmd NativeBuildOptions
  | NativeInstallCmd NativeInstallOptions
  | NativeCleanCmd
  | NativeDoctorCmd
  deriving (Eq, Show)

data NativeBuildOptions = NativeBuildOptions
  { profile :: Maybe String
  , tool :: Maybe ToolChoice
  , watch :: Bool
  , jobs :: Maybe Int
  , outDir :: Maybe FilePath
  , targetPlatform :: Maybe String
  , passthrough :: [String]
  }
  deriving (Eq, Show)

data NativeInstallOptions = NativeInstallOptions
  { easywordyRoot :: Maybe FilePath
  , installMode :: Maybe InstallMode
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Enums
--------------------------------------------------------------------------------

data ColorMode
  = ColorAuto
  | ColorAlways
  | ColorNever
  deriving (Eq, Show)

data ReportMode
  = ReportHuman
  | ReportJson
  deriving (Eq, Show)

data OutputFormat
  = FormatText
  | FormatJson
  | FormatDot
  | FormatMermaid
  | FormatHtml
  deriving (Eq, Show)

data MetadataFormat
  = MetadataJson
  deriving (Eq, Show)

data TimingMode
  = TimingHtml
  | TimingJson
  deriving (Eq, Show)

data CoverageMode
  = CoverageOff
  | CoverageText
  | CoverageJson
  | CoverageLcov
  deriving (Eq, Show)

data SourceMapMode
  = SourceMapNone
  | SourceMapInline
  | SourceMapExternal
  deriving (Eq, Show)

data MinifyMode
  = MinifyDefault
  | MinifyEnabled
  | MinifyDisabled
  deriving (Eq, Show)

data IncludeMode
  = IncludeAuto
  | IncludeAlways
  | IncludeNever
  deriving (Eq, Show)

data LiveReloadMode
  = LiveReloadDefault
  | LiveReloadEnabled
  | LiveReloadDisabled
  deriving (Eq, Show)

data OpenMode
  = OpenNever
  | OpenDefault
  | OpenPath FilePath
  deriving (Eq, Show)

data RuntimeTarget
  = RuntimeBrowser
  | RuntimeSsr
  deriving (Eq, Show)

data ScaffoldKind
  = ScaffoldProject
  | ScaffoldWorkspace
  | ScaffoldWapp
  deriving (Eq, Show)

data InstallMode
  = InstallCopy
  | InstallLink
  deriving (Eq, Show)

data RestartMode
  = RestartNone
  | RestartTouch
  | RestartCommand
  deriving (Eq, Show)

data ToolChoice
  = ToolAuto
  | ToolStack
  | ToolCabal
  deriving (Eq, Show)

data Shell
  = ShellBash
  | ShellZsh
  | ShellFish
  | ShellPowerShell
  | ShellElvish
  deriving (Eq, Show)

data EmitItem
  = EmitJs
  | EmitJsMap
  | EmitCss
  | EmitAssetManifest
  | EmitWappLayout
  | EmitMeta
  | EmitCst
  | EmitAst
  | EmitSymbols
  | EmitModuleGraph
  | EmitIr String
  | EmitJsIr
  deriving (Eq, Show)

data EntryRequirement
  = EntryOptional
  | EntryRequired
  deriving (Eq, Show)


data TokenState = Normal | InSingle | InDouble | EscapeNormal | EscapeDouble
