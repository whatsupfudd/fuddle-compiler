{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Options.Cli.Dispatch
  ( DispatchError(..)
  , CliHandlers(..)

  , normalizeCli
  , dispatchCli
  , renderDispatchErrors
  ) where

import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe, isJust)

import Options.Cli.Types
  ( CheckOptions(..)
  , CleanOptions(..)
  , Cli(..)
  , Command(..)
  , DepsCommand(..)
  , DevOptions(..)
  , MemberSelection(..)
  , FmtOptions(..)
  , GlobalOptions(..)
  , HelpOptions(..)
  , IncludeMode(..)
  , InspectCommand(..)
  , InspectDumpOptions(..)
  , InspectGraphOptions(..)
  , InspectIrOptions(..)
  , InspectSubject(..)
  , InstallMode(..)
  , BuildOptions(..)
  , MetadataOptions(..)
  , NativeBuildOptions(..)
  , NativeCommand(..)
  , NativeInstallOptions(..)
  , OpenMode(..)
  , PreviewOptions(..)
  , ReplOptions(..)
  , ResolutionOptions(..)
  , RestartMode(..)
  , Selection(..)
  , TestOptions(..)
  , WappCommand(..)
  , WappInstallOptions(..)
  , WappStageOptions(..)
  , WappLayoutOptions(..)
  , DoctorOptions(..)
  , ExplainOptions(..)
  , CompletionsOptions(..)
  , InitOptions(..)
  , NewOptions(..)
  )

--------------------------------------------------------------------------------
-- Dispatcher-facing types
--------------------------------------------------------------------------------

data DispatchError = DispatchError
  { path :: [String]
  , message :: String
  }
  deriving (Eq, Show)

data CliHandlers m a = CliHandlers
  { onBuild :: GlobalOptions -> BuildOptions -> m a
  , onCheck :: GlobalOptions -> CheckOptions -> m a
  , onDev :: GlobalOptions -> DevOptions -> m a
  , onPreview :: GlobalOptions -> PreviewOptions -> m a
  , onTest :: GlobalOptions -> TestOptions -> m a
  , onFmt :: GlobalOptions -> FmtOptions -> m a
  , onRepl :: GlobalOptions -> ReplOptions -> m a
  , onClean :: GlobalOptions -> CleanOptions -> m a
  , onInit :: GlobalOptions -> InitOptions -> m a
  , onNew :: GlobalOptions -> NewOptions -> m a
  , onDeps :: GlobalOptions -> DepsCommand -> m a
  , onMetadata :: GlobalOptions -> MetadataOptions -> m a
  , onInspect :: GlobalOptions -> InspectCommand -> m a
  , onDoctor :: GlobalOptions -> DoctorOptions -> m a
  , onExplain :: GlobalOptions -> ExplainOptions -> m a
  , onCompletions :: GlobalOptions -> CompletionsOptions -> m a
  , onHelp :: GlobalOptions -> HelpOptions -> m a
  , onVersion :: GlobalOptions -> m a
  , onWapp :: GlobalOptions -> WappCommand -> m a
  , onNative :: GlobalOptions -> NativeCommand -> m a
  }

--------------------------------------------------------------------------------
-- Public entry points
--------------------------------------------------------------------------------

normalizeCli :: Cli -> Either [DispatchError] Cli
normalizeCli cli =
  let
    (globalsNorm, globalsErrs) = normalizeGlobals cli.globals
    (commandNorm, commandErrs) = normalizeCommand cli.command
    errs = globalsErrs <> commandErrs
    cliNorm = Cli { globals = globalsNorm, command = commandNorm }
  in
  if null errs then 
    Right cliNorm
  else
    Left errs


dispatchCli :: CliHandlers m a -> Cli -> Either [DispatchError] (m a)
dispatchCli handlers cli = do
  cliNorm <- normalizeCli cli
  pure $ case cliNorm.command of
    BuildCmd opts -> handlers.onBuild cliNorm.globals opts
    CheckCmd opts -> handlers.onCheck cliNorm.globals opts
    DevCmd opts -> handlers.onDev cliNorm.globals opts
    PreviewCmd opts -> handlers.onPreview cliNorm.globals opts
    TestCmd opts -> handlers.onTest cliNorm.globals opts
    FmtCmd opts -> handlers.onFmt cliNorm.globals opts
    ReplCmd opts -> handlers.onRepl cliNorm.globals opts
    CleanCmd opts -> handlers.onClean cliNorm.globals opts
    InitCmd opts -> handlers.onInit cliNorm.globals opts
    NewCmd opts -> handlers.onNew cliNorm.globals opts
    DepsCmd depsCmd -> handlers.onDeps cliNorm.globals depsCmd
    MetadataCmd opts -> handlers.onMetadata cliNorm.globals opts
    InspectCmd inspectCmd -> handlers.onInspect cliNorm.globals inspectCmd
    DoctorCmd opts -> handlers.onDoctor cliNorm.globals opts
    ExplainCmd opts -> handlers.onExplain cliNorm.globals opts
    CompletionsCmd opts -> handlers.onCompletions cliNorm.globals opts
    HelpCmd opts -> handlers.onHelp cliNorm.globals opts
    VersionCmd -> handlers.onVersion cliNorm.globals
    WappCmd wappCmd -> handlers.onWapp cliNorm.globals wappCmd
    NativeCmd nativeCmd -> handlers.onNative cliNorm.globals nativeCmd


renderDispatchErrors :: [DispatchError] -> String
renderDispatchErrors errs =
  intercalate "\n" (fmap renderOne errs)
  where
  renderOne :: DispatchError -> String
  renderOne err =
    let
      prefix = case err.path of
        [] -> "error"
        ps -> unwords ps
    in
    prefix <> ": " <> err.message

--------------------------------------------------------------------------------
-- Normalization
--------------------------------------------------------------------------------

normalizeGlobals :: GlobalOptions -> (GlobalOptions, [DispatchError])
normalizeGlobals opts =
  let errs =
        catMaybes
          [ if opts.quiet && opts.verbosity > 0
              then Just (dispatchErr ["global"] "Cannot combine --quiet with one or more --verbose flags.")
              else Nothing
          ]
  in (opts, errs)

normalizeCommand :: Command -> (Command, [DispatchError])
normalizeCommand = \case
  BuildCmd opts ->
    let (optsNorm, errs) = normalizeBuildOptions opts
    in (BuildCmd optsNorm, errs)

  CheckCmd opts ->
    let (optsNorm, errs) = normalizeCheckOptions opts
    in (CheckCmd optsNorm, errs)

  DevCmd opts ->
    let (optsNorm, errs) = normalizeDevOptions opts
    in (DevCmd optsNorm, errs)

  PreviewCmd opts ->
    let (optsNorm, errs) = normalizePreviewOptions opts
    in (PreviewCmd optsNorm, errs)

  TestCmd opts ->
    let (optsNorm, errs) = normalizeTestOptions opts
    in (TestCmd optsNorm, errs)

  FmtCmd opts ->
    let (optsNorm, errs) = normalizeFmtOptions opts
    in (FmtCmd optsNorm, errs)

  ReplCmd opts ->
    (ReplCmd opts, [])

  CleanCmd opts ->
    let (optsNorm, errs) = normalizeCleanOptions opts
    in (CleanCmd optsNorm, errs)

  InitCmd opts ->
    (InitCmd opts, [])

  NewCmd opts ->
    (NewCmd opts, [])

  DepsCmd depsCmd ->
    (DepsCmd depsCmd, [])

  MetadataCmd opts ->
    (MetadataCmd opts, [])

  InspectCmd inspectCmd ->
    let (inspectCmdNorm, errs) = normalizeInspectCommand inspectCmd
    in (InspectCmd inspectCmdNorm, errs)

  DoctorCmd opts ->
    (DoctorCmd opts, [])

  ExplainCmd opts ->
    (ExplainCmd opts, [])

  CompletionsCmd opts ->
    (CompletionsCmd opts, [])

  HelpCmd opts ->
    (HelpCmd opts, [])

  VersionCmd ->
    (VersionCmd, [])

  WappCmd wappCmd ->
    let (wappCmdNorm, errs) = normalizeWappCommand wappCmd
    in (WappCmd wappCmdNorm, errs)

  NativeCmd nativeCmd ->
    let (nativeCmdNorm, errs) = normalizeNativeCommand nativeCmd
    in (NativeCmd nativeCmdNorm, errs)

--------------------------------------------------------------------------------
-- Build / Check / Dev / Preview / Test
--------------------------------------------------------------------------------

normalizeBuildOptions :: BuildOptions -> (BuildOptions, [DispatchError])
normalizeBuildOptions opts =
  let resolutionNorm = normalizeResolution opts.resolution
      (profileNorm, profileErrs) =
        normalizeProfileWithRelease ["build"] opts.profile opts.release "release"

      errs =
        profileErrs
          <> catMaybes
              [ if opts.stdout && isJust opts.output
                  then Just (dispatchErr ["build"] "Cannot combine --stdout with --output.")
                  else Nothing
              ]

      optsNorm =
        BuildOptions
          { selection = opts.selection
          , profile = profileNorm
          , release = opts.release
          , optimize = opts.optimize
          , resolution = resolutionNorm
          , lints = opts.lints
          , timings = opts.timings
          , output = opts.output
          , outDir = opts.outDir
          , stdout = opts.stdout
          , emit = opts.emit
          , sourceMap = opts.sourceMap
          , minify = opts.minify
          , basePath = opts.basePath
          , withAssets = opts.withAssets
          , withNatives = opts.withNatives
          }
  in (optsNorm, errs)

normalizeCheckOptions :: CheckOptions -> (CheckOptions, [DispatchError])
normalizeCheckOptions opts =
  let resolutionNorm = normalizeResolution opts.resolution
      profileNorm = Just (fromMaybe "dev" opts.profile)

      errs =
        checkStdinSelectionErrs ["check"] opts.stdin opts.stdinFilepath opts.selection

      optsNorm =
        CheckOptions
          { selection = opts.selection
          , profile = profileNorm
          , resolution = resolutionNorm
          , lints = opts.lints
          , full = opts.full
          , stdin = opts.stdin
          , stdinFilepath = opts.stdinFilepath
          }
  in (optsNorm, errs)

normalizeDevOptions :: DevOptions -> (DevOptions, [DispatchError])
normalizeDevOptions opts =
  let resolutionNorm = normalizeResolution opts.resolution
      profileNorm = Just (fromMaybe "dev" opts.profile)

      optsNorm =
        DevOptions
          { selection = opts.selection
          , profile = profileNorm
          , resolution = resolutionNorm
          , host = opts.host
          , port = opts.port
          , strictPort = opts.strictPort
          , open = opts.open
          , clearScreen = opts.clearScreen
          , pollMs = opts.pollMs
          , debounceMs = opts.debounceMs
          , liveReload = opts.liveReload
          , proxyUrl = opts.proxyUrl
          , easywordyRoot = opts.easywordyRoot
          , easywordyUrl = opts.easywordyUrl
          , withAssets = opts.withAssets
          , withNatives = opts.withNatives
          , fullRebuild = opts.fullRebuild
          , basePath = opts.basePath
          }
  in (optsNorm, [])

normalizePreviewOptions :: PreviewOptions -> (PreviewOptions, [DispatchError])
normalizePreviewOptions opts =
  (opts, [])

normalizeTestOptions :: TestOptions -> (TestOptions, [DispatchError])
normalizeTestOptions opts =
  let resolutionNorm = normalizeResolution opts.resolution
      profileNorm = Just (fromMaybe "ci" opts.profile)

      optsNorm =
        TestOptions
          { selection = opts.selection
          , profile = profileNorm
          , resolution = resolutionNorm
          , filterExpr = opts.filterExpr
          , watch = opts.watch
          , noRun = opts.noRun
          , coverage = opts.coverage
          , updateSnapshots = opts.updateSnapshots
          , report = opts.report
          }
  in (optsNorm, [])

--------------------------------------------------------------------------------
-- Fmt / Clean
--------------------------------------------------------------------------------

normalizeFmtOptions :: FmtOptions -> (FmtOptions, [DispatchError])
normalizeFmtOptions opts =
  let errs =
        catMaybes
          [ if opts.stdin && not (null opts.files)
              then Just (dispatchErr ["fmt"] "Cannot combine --stdin with positional file arguments.")
              else Nothing
          , if isJust opts.stdinFilepath && not opts.stdin
              then Just (dispatchErr ["fmt"] "--stdin-filepath requires --stdin.")
              else Nothing
          , if opts.check && opts.write
              then Just (dispatchErr ["fmt"] "Cannot combine --check with --write.")
              else Nothing
          ]

      writeNorm =
        if opts.stdin
          then opts.write
          else
            if null opts.files
              then opts.write
              else
                if opts.check || opts.write
                  then opts.write
                  else True

      optsNorm =
        FmtOptions
          { files = opts.files
          , check = opts.check
          , write = writeNorm
          , stdin = opts.stdin
          , stdinFilepath = opts.stdinFilepath
          , report = opts.report
          }
  in (optsNorm, errs)

normalizeCleanOptions :: CleanOptions -> (CleanOptions, [DispatchError])
normalizeCleanOptions opts =
  let anySpecific =
        opts.artifacts || opts.cache || opts.deps || opts.natives || opts.allClean

      activateAll =
        opts.allClean || not anySpecific

      artifactsNorm = activateAll || opts.artifacts
      cacheNorm = activateAll || opts.cache
      depsNorm = activateAll || opts.deps
      nativesNorm = activateAll || opts.natives
      allNorm = activateAll

      optsNorm =
        CleanOptions
          { artifacts = artifactsNorm
          , cache = cacheNorm
          , deps = depsNorm
          , natives = nativesNorm
          , allClean = allNorm
          }
  in (optsNorm, [])

--------------------------------------------------------------------------------
-- Inspect
--------------------------------------------------------------------------------

normalizeInspectCommand :: InspectCommand -> (InspectCommand, [DispatchError])
normalizeInspectCommand = \case
  InspectCstCmd opts ->
    let (optsNorm, errs) = normalizeInspectDumpOptions ["inspect", "cst"] opts
    in (InspectCstCmd optsNorm, errs)

  InspectAstCmd opts ->
    let (optsNorm, errs) = normalizeInspectDumpOptions ["inspect", "ast"] opts
    in (InspectAstCmd optsNorm, errs)

  InspectSymbolsCmd opts ->
    let (optsNorm, errs) = normalizeInspectDumpOptions ["inspect", "symbols"] opts
    in (InspectSymbolsCmd optsNorm, errs)

  InspectGraphCmd opts ->
    (InspectGraphCmd opts, [])

  InspectIrCmd opts ->
    let (subjectNorm, errs) = normalizeInspectSubject ["inspect", "ir"] opts.subject
        optsNorm =
          InspectIrOptions
            { subject = subjectNorm
            , stage = opts.stage
            , format = opts.format
            , output = opts.output
            }
    in (InspectIrCmd optsNorm, errs)

  InspectJsCmd opts ->
    let (optsNorm, errs) = normalizeInspectDumpOptions ["inspect", "js"] opts
    in (InspectJsCmd optsNorm, errs)

normalizeInspectDumpOptions :: [String] -> InspectDumpOptions -> (InspectDumpOptions, [DispatchError])
normalizeInspectDumpOptions path opts =
  let (subjectNorm, errs) = normalizeInspectSubject path opts.subject
      optsNorm =
        InspectDumpOptions
          { subject = subjectNorm
          , format = opts.format
          , output = opts.output
          }
  in (optsNorm, errs)

normalizeInspectSubject :: [String] -> InspectSubject -> (InspectSubject, [DispatchError])
normalizeInspectSubject path opts =
  let (entryNorm, entryErrs) =
        case (opts.entry, opts.positional) of
          (Nothing, Nothing) ->
            (Nothing, [])

          (Just entryPath, Nothing) ->
            (Just entryPath, [])

          (Nothing, Just positionalPath) ->
            (Just positionalPath, [])

          (Just _, Just _) ->
            ( opts.entry
            , [dispatchErr path "Cannot specify both a positional entry path and --entry."]
            )

      primaryInputCount =
        countTrue
          [ isJust opts.target
          , isJust entryNorm
          , opts.stdin
          ]

      selectionErrs =
        catMaybes
          [ if primaryInputCount > 1
              then Just (dispatchErr path "Choose exactly one primary inspect subject: --target, entry path, or --stdin.")
              else Nothing
          , if isJust opts.stdinFilepath && not opts.stdin
              then Just (dispatchErr path "--stdin-filepath requires --stdin.")
              else Nothing
          , if opts.stdin && not (memberSelectionEmpty opts.members)
              then Just (dispatchErr path "Cannot combine --stdin with --workspace or --package.")
              else Nothing
          ]

      optsNorm =
        InspectSubject
          { target = opts.target
          , entry = entryNorm
          , positional = Nothing
          , stdin = opts.stdin
          , stdinFilepath = opts.stdinFilepath
          , members = opts.members
          }
  in (optsNorm, entryErrs <> selectionErrs)

--------------------------------------------------------------------------------
-- Wapp / Native
--------------------------------------------------------------------------------

normalizeWappCommand :: WappCommand -> (WappCommand, [DispatchError])
normalizeWappCommand = \case
  WappStageCmd opts ->
    let errs =
          catMaybes
            [ if opts.copyAssets && opts.linkAssets
                then Just (dispatchErr ["wapp", "stage"] "Cannot combine --copy-assets with --link-assets.")
                else Nothing
            ]
    in (WappStageCmd opts, errs)

  WappInstallCmd opts ->
    let installModeNorm = Just (fromMaybe InstallCopy opts.installMode)
        optsNorm =
          WappInstallOptions
            { easywordyRoot = opts.easywordyRoot
            , profile = opts.profile
            , installMode = installModeNorm
            , withAssets = opts.withAssets
            , withNatives = opts.withNatives
            , restart = opts.restart
            , dryRun = opts.dryRun
            }
    in (WappInstallCmd optsNorm, [])

  WappLayoutCmd opts ->
    (WappLayoutCmd opts, [])

normalizeNativeCommand :: NativeCommand -> (NativeCommand, [DispatchError])
normalizeNativeCommand = \case
  NativeBuildCmd opts ->
    (NativeBuildCmd opts, [])

  NativeInstallCmd opts ->
    let installModeNorm = Just (fromMaybe InstallCopy opts.installMode)
        optsNorm =
          NativeInstallOptions
            { easywordyRoot = opts.easywordyRoot
            , installMode = installModeNorm
            }
    in (NativeInstallCmd optsNorm, [])

  NativeCleanCmd ->
    (NativeCleanCmd, [])

  NativeDoctorCmd ->
    (NativeDoctorCmd, [])

--------------------------------------------------------------------------------
-- Shared helpers
--------------------------------------------------------------------------------

normalizeResolution :: ResolutionOptions -> ResolutionOptions
normalizeResolution opts =
  let lockedNorm = opts.locked || opts.frozen
      offlineNorm = opts.offline || opts.frozen
  in
    ResolutionOptions
      { jobs = opts.jobs
      , keepGoing = opts.keepGoing
      , offline = offlineNorm
      , locked = lockedNorm
      , frozen = opts.frozen
      , targetDir = opts.targetDir
      , cacheDir = opts.cacheDir
      }

normalizeProfileWithRelease
  :: [String]
  -> Maybe String
  -> Bool
  -> String
  -> (Maybe String, [DispatchError])
normalizeProfileWithRelease path profileMb releaseFlag defaultProfile =
  case (releaseFlag, profileMb) of
    (False, Nothing) ->
      (Just defaultProfile, [])

    (False, Just profileName) ->
      (Just profileName, [])

    (True, Nothing) ->
      (Just "release", [])

    (True, Just profileName) ->
      if isReleaseProfile profileName
        then (Just profileName, [])
        else
          ( Just profileName
          , [dispatchErr path "Cannot combine --release with a non-release --profile value."]
          )

checkStdinSelectionErrs
  :: [String]
  -> Bool
  -> Maybe FilePath
  -> Selection
  -> [DispatchError]
checkStdinSelectionErrs path stdinFlag stdinFilepathMb selection =
  catMaybes
    [ if isJust stdinFilepathMb && not stdinFlag
        then Just (dispatchErr path "--stdin-filepath requires --stdin.")
        else Nothing
    , if stdinFlag && not (selectionEmpty selection)
        then Just (dispatchErr path "Cannot combine --stdin with targets, entries, --workspace, or --package.")
        else Nothing
    ]

selectionEmpty :: Selection -> Bool
selectionEmpty selection =
     null selection.targets
  && not selection.allTargets
  && null selection.entries
  && memberSelectionEmpty selection.members

memberSelectionEmpty :: MemberSelectionLike a => a -> Bool
memberSelectionEmpty members =
     not (getWorkspace members)
  && null (getPackages members)

class MemberSelectionLike a where
  getWorkspace :: a -> Bool
  getPackages :: a -> [String]

instance MemberSelectionLike Selection where
  getWorkspace selection =
    selection.members.workspace

  getPackages selection =
    selection.members.packages

instance MemberSelectionLike InspectSubject where
  getWorkspace _ =
    False

  getPackages _ =
    []

instance MemberSelectionLike InspectSubjectMembersProxy where
  getWorkspace proxy =
    proxy.workspace

  getPackages proxy =
    proxy.packages

data InspectSubjectMembersProxy = InspectSubjectMembersProxy
  { workspace :: Bool
  , packages :: [String]
  }

instance MemberSelectionLike MemberSelection where
  getWorkspace members =
    members.workspace

  getPackages members =
    members.packages

isReleaseProfile :: String -> Bool
isReleaseProfile profileName =
  lowercaseTrim profileName == "release"

lowercaseTrim :: String -> String
lowercaseTrim =
  fmap lowerAscii . trim
  where
    lowerAscii c =
      if 'A' <= c && c <= 'Z'
        then toEnum (fromEnum c + 32)
        else c

trim :: String -> String
trim =
  dropWhile isSpaceAsciiEnd . reverse . dropWhile isSpaceAsciiEnd . reverse
  where
    isSpaceAsciiEnd c =
      c == ' ' || c == '\n' || c == '\r' || c == '\t'

countTrue :: [Bool] -> Int
countTrue =
  length . filter id

dispatchErr :: [String] -> String -> DispatchError
dispatchErr path message =
  DispatchError
    { path = path
    , message = message
    }