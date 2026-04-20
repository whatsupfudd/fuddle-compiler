{-# LANGUAGE DerivingStrategies #-}

module Fuddle.Compiler.ModGraph.Discover
  ( ReqDiscoverMod(..)
  , ResDiscoverMod(..)
  , CandidateMod(..)
  , ErrDiscoverMod(..)
  , discoverMods
  ) where

import Control.Exception (IOException, displayException, try)

import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.List (sort)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word, Word8, Word64)

import System.Directory
  ( canonicalizePath
  , doesDirectoryExist
  , doesFileExist
  , doesPathExist
  , listDirectory
  , pathIsSymbolicLink
  )
import System.FilePath ((</>), takeExtension)


import Fuddle.Compiler.Base.Core (Hash64(..))
import Fuddle.Compiler.Base.Diag (Diag)
import Fuddle.Compiler.ModGraph.Name (ModName)
import Fuddle.Compiler.ModGraph.Origin
  ( OriginMod(..)
  , PkgRefMod(..)
  , RootKindMod(..)
  , SourceLocMod(..)
  )
import Fuddle.Compiler.ModGraph.Types
  ( PkgId(..)
  , RootId(..)
  )
import Fuddle.Compiler.ModGraph.GraphTypes (RootModGraph (..))


data CandidateMod = CandidateMod
  { origin :: !OriginMod
  , nameMay :: !(Maybe ModName)
  , hashSource :: !Hash64
  }
  deriving stock (Eq, Show)

data ReqDiscoverMod = ReqDiscoverMod
  { roots :: !(Vector RootModGraph)
  , includeDeps :: !Bool
  }
  deriving stock (Eq, Show)

data ResDiscoverMod = ResDiscoverMod
  { cands :: !(Vector CandidateMod)
  , diags :: !(Vector Diag)
  }
  deriving stock (Eq, Show)

data ErrDiscoverMod
  = RootMissingErrDiscoverMod !RootId
  | ManifestInvalidErrDiscoverMod !RootId !Text
  | RegistryUnavailableErrDiscoverMod !RootId !Text
  deriving stock (Eq, Show)

discoverMods :: ReqDiscoverMod -> IO (Either ErrDiscoverMod ResDiscoverMod)
discoverMods req = do
  let rootsAct = filter (rootActiveDisc req.includeDeps) (V.toList req.roots)
  rootsRes <- mapM normalizeRootDisc rootsAct
  case sequence rootsRes of
    Left err -> pure (Left err)
    Right rootsCan ->
      case validateRootsDisc rootsCan of
        Left err -> pure (Left err)
        Right rootsOk -> do
          candsRes <- mapM discoverRootDisc rootsOk
          case sequence candsRes of
            Left err -> pure (Left err)
            Right candss ->
              let
                cands1 = dedupCandidatesDisc (concat candss)
              in
              pure
                (Right
                  ResDiscoverMod
                    { cands = V.fromList cands1
                    , diags = V.empty
                    })

rootActiveDisc :: Bool -> RootModGraph -> Bool
rootActiveDisc includeDeps0 root0 =
  case root0.kind of
    WorkspaceRootMod -> True
    VirtualRootMod -> True
    PackageRootMod -> includeDeps0
    RegistryRootMod -> includeDeps0

normalizeRootDisc :: RootModGraph -> IO (Either ErrDiscoverMod RootModGraph)
normalizeRootDisc root0 =
  case root0.kind of
    VirtualRootMod -> pure (Right root0)
    _ -> do
      existsYes <- doesPathExist root0.path
      if not existsYes
        then pure (Left (missingRootErrDisc root0))
        else do
          pathRes <- try (canonicalizePath root0.path) :: IO (Either IOException FilePath)
          pure
            (case pathRes of
              Left err -> Left (ioErrRootDisc root0 err)
              Right pathCan -> Right root0 { path = pathCan })

validateRootsDisc :: [RootModGraph] -> Either ErrDiscoverMod [RootModGraph]
validateRootsDisc roots0 =
  case dupByDisc (\root1 -> root1.uid) roots0 of
    Just rootDup ->
      Left
        (ManifestInvalidErrDiscoverMod
          rootDup.uid
          "duplicate root id in discovery request")
    Nothing ->
      case dupByDisc (\root1 -> root1.path) roots0 of
        Just rootDup ->
          Left
            (ManifestInvalidErrDiscoverMod
              rootDup.uid
              ("duplicate root path in discovery request: " <> T.pack rootDup.path))
        Nothing -> Right roots0

discoverRootDisc :: RootModGraph -> IO (Either ErrDiscoverMod [CandidateMod])
discoverRootDisc root0 =
  case root0.kind of
    VirtualRootMod -> pure (Right [])
    _ -> do
      res <- try (discoverRootPathDisc root0) :: IO (Either IOException [CandidateMod])
      pure
        (case res of
          Left err -> Left (ioErrRootDisc root0 err)
          Right cands0 -> Right cands0)

discoverRootPathDisc :: RootModGraph -> IO [CandidateMod]
discoverRootPathDisc root0 = do
  dirYes <- doesDirectoryExist root0.path
  if dirYes
    then do
      files <- scanDirDisc root0.path
      mapM (candidateFileDisc root0) files
    else do
      fileYes <- doesFileExist root0.path
      if fileYes
        then
          if sourceFileDisc root0.path
            then do
              pathCan <- canonicalizePath root0.path
              cand <- candidateFileDisc root0 pathCan
              pure [cand]
            else pure []
        else ioError (userError ("unsupported root path shape: " <> root0.path))

scanDirDisc :: FilePath -> IO [FilePath]
scanDirDisc path0 = do
  names <- sort <$> listDirectory path0
  fmap concat (mapM (scanEntryDisc path0) names)

scanEntryDisc :: FilePath -> FilePath -> IO [FilePath]
scanEntryDisc parent0 name0 = do
  let path0 = parent0 </> name0
  dirYes <- doesDirectoryExist path0
  if dirYes
    then do
      linkYes <- pathIsSymbolicLink path0
      if linkYes
        then pure []
        else scanDirDisc path0
    else do
      fileYes <- doesFileExist path0
      if fileYes && sourceFileDisc path0
        then do
          pathCan <- canonicalizePath path0
          pure [pathCan]
        else pure []

candidateFileDisc :: RootModGraph -> FilePath -> IO CandidateMod
candidateFileDisc root0 path0 = do
  src <- BS.readFile path0
  pure
    CandidateMod
      { origin =
          OriginMod
            { root = root0.uid
            , rootKind = root0.kind
            , pkg = pkgRootDisc root0
            , loc = FileSourceLocMod path0
            }
      , nameMay = Nothing
      , hashSource = hashBytesDisc src
      }

pkgRootDisc :: RootModGraph -> PkgRefMod
pkgRootDisc root0 =
  case root0.pkgMay of
    Just pkg0 -> pkg0
    Nothing ->
      PkgRefMod
        { uid = pkgFallbackDisc root0.uid
        , name = root0.name
        , version = "workspace"
        }

pkgFallbackDisc :: RootId -> PkgId
pkgFallbackDisc (RootId n0) = PkgId n0

missingRootErrDisc :: RootModGraph -> ErrDiscoverMod
missingRootErrDisc root0 =
  case root0.kind of
    RegistryRootMod ->
      RegistryUnavailableErrDiscoverMod
        root0.uid
        ("registry root is missing: " <> T.pack root0.path)
    _ -> RootMissingErrDiscoverMod root0.uid

ioErrRootDisc :: RootModGraph -> IOException -> ErrDiscoverMod
ioErrRootDisc root0 err0 =
  case root0.kind of
    RegistryRootMod ->
      RegistryUnavailableErrDiscoverMod root0.uid (T.pack (displayException err0))
    _ ->
      ManifestInvalidErrDiscoverMod root0.uid (T.pack (displayException err0))

sourceFileDisc :: FilePath -> Bool
sourceFileDisc path0 =
  let ext0 = takeExtension path0
  in ext0 == ".fud" || ext0 == ".fuddle"


dedupCandidatesDisc :: [CandidateMod] -> [CandidateMod]
dedupCandidatesDisc = iterCand Set.empty []
  where
    iterCand :: Set.Set SourceLocMod -> [CandidateMod] -> [CandidateMod] -> [CandidateMod]
    iterCand seen0 acc0 [] = reverse acc0
    iterCand seen0 acc0 (cand0 : rest0) =
      let loc0 = cand0.origin.loc
      in
      if Set.member loc0 seen0
        then iterCand seen0 acc0 rest0
        else iterCand (Set.insert loc0 seen0) (cand0 : acc0) rest0


dupByDisc :: Ord key => (a -> key) -> [a] -> Maybe a
dupByDisc key0 = go Set.empty
  where
    go _ [] = Nothing
    go seen0 (item0 : rest0) =
      let key1 = key0 item0
      in
      if Set.member key1 seen0
        then Just item0
        else go (Set.insert key1 seen0) rest0


hashBytesDisc :: BS.ByteString -> Hash64
hashBytesDisc bytes0 = Hash64 (BS.foldl' stepDisc offsetDisc bytes0)

offsetDisc :: Word64
offsetDisc = fromIntegral (14695981039346656037 :: Integer)

primeDisc :: Word64
primeDisc = fromIntegral (1099511628211 :: Integer)

stepDisc :: Word64 -> Word8 -> Word64
stepDisc acc0 byte0 = (acc0 `xor` fromIntegral byte0) * primeDisc