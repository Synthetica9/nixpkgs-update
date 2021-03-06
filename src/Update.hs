{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Update
  ( updateAll
  ) where

import qualified Blacklist
import qualified Check
import Clean (fixSrcUrl)
import Control.Category ((>>>))
import Control.Error
import Control.Exception (SomeException, throw, toException)
import Control.Monad (forM_)
import Data.Function ((&))
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
import qualified File
import qualified GH
import qualified Git
import NeatInterpolation (text)
import qualified Nix
import Outpaths
import Prelude hiding (FilePath)
import Shelly
import Utils
  ( ExitCode(..)
  , Options(..)
  , UpdateEnv(..)
  , Version
  , branchName
  , canFail
  , checkAttrPathVersion
  , eitherToError
  , orElse
  , ourShell
  , parseUpdates
  , rewriteError
  , setupNixpkgs
  , shE
  , tRead
  )

default (T.Text)

data MergeBaseOutpathsInfo = MergeBaseOutpathsInfo
  { lastUpdated :: UTCTime
  , mergeBaseOutpaths :: Set ResultLine
  }

errorExit' :: (Text -> Sh ()) -> Text -> Text -> Sh a
errorExit' log branchName message = do
  Git.cleanup branchName
  log message
  throw (ExitCode 1)

log' logFile msg
    -- TODO: switch to Data.Time.Format.ISO8601 once time-1.9.0 is available
 = do
  runDate <-
    T.pack . formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) <$>
    liftIO getCurrentTime
  appendfile logFile (runDate <> " " <> msg <> "\n")

updateAll :: Options -> IO ()
updateAll options =
  ourShell options $ do
    let logFile = fromText (workingDir options) </> "ups.log"
    mkdir_p (fromText (workingDir options))
    touchfile logFile
    updates <- readfile "packages-to-update.txt"
    let log = log' logFile
    appendfile logFile "\n\n"
    log "New run of ups.sh"
    twoHoursAgo <- liftIO $ addUTCTime (fromInteger $ -60 * 60 * 2) <$> getCurrentTime
    mergeBaseOutpathSet <- liftIO $ newIORef (MergeBaseOutpathsInfo twoHoursAgo S.empty)
    updateLoop options log (parseUpdates updates) mergeBaseOutpathSet

updateLoop ::
     Options
  -> (Text -> Sh ())
  -> [Either Text (Text, Version, Version)]
  -> IORef MergeBaseOutpathsInfo
  -> Sh ()
updateLoop _ log [] _ = log "ups.sh finished"
updateLoop options log (Left e:moreUpdates) mergeBaseOutpathsContext = do
  log e
  updateLoop options log moreUpdates mergeBaseOutpathsContext
updateLoop options log (Right (package, oldVersion, newVersion):moreUpdates) mergeBaseOutpathsContext = do
  log (package <> " " <> oldVersion <> " -> " <> newVersion)
  updated <-
    catch_sh
      (updatePackage log (UpdateEnv package oldVersion newVersion options) mergeBaseOutpathsContext)
      (\case
         ExitCode 0 -> return True
         ExitCode _ -> return False)
  if updated
    then do
      log "SUCCESS"
      updateLoop options log moreUpdates mergeBaseOutpathsContext
    else do
      log "FAIL"
      if ".0" `T.isSuffixOf` newVersion
        then let Just newNewVersion = ".0" `T.stripSuffix` newVersion
              in updateLoop
                   options
                   log
                   (Right (package, oldVersion, newNewVersion) : moreUpdates)
                   mergeBaseOutpathsContext
        else updateLoop options log moreUpdates mergeBaseOutpathsContext

updatePackage :: (Text -> Sh ()) -> UpdateEnv -> IORef MergeBaseOutpathsInfo -> Sh Bool
updatePackage log updateEnv mergeBaseOutpathsContext = do
  let errorExit = errorExit' log (branchName updateEnv)
  eitherToError errorExit (pure (Blacklist.packageName (packageName updateEnv)))
  setupNixpkgs
  -- Check whether requested version is newer than the current one
  eitherToError errorExit (Nix.compareVersions updateEnv)
  Git.fetchIfStale
  whenM
    (Git.autoUpdateBranchExists (packageName updateEnv))
    (errorExit "Update branch already on origin.")
  Git.cleanAndResetToMaster
  attrPath <- eitherToError errorExit (Nix.lookupAttrPath updateEnv)
  srcUrls <- eitherToError errorExit (Nix.getSrcUrls attrPath)
  eitherToError errorExit (pure (Blacklist.srcUrl srcUrls))
  eitherToError errorExit (pure (Blacklist.attrPath attrPath))
  derivationFile <-
    eitherToError errorExit (Nix.getDerivationFile updateEnv attrPath)
  flip
    catches_sh
    [ ShellyHandler (\(ex :: ExitCode) -> throw ex)
    , ShellyHandler (\(ex :: SomeException) -> errorExit (T.pack (show ex)))
    ] $ do
    unless (checkAttrPathVersion attrPath (newVersion updateEnv)) $
      errorExit
        ("Version in attr path " <> attrPath <> " not compatible with " <>
         newVersion updateEnv)
    -- Make sure it hasn't been updated on master
    masterDerivationContents <- readfile derivationFile
    eitherToError
      errorExit
      (Nix.oldVersionOn updateEnv "master" masterDerivationContents)
    -- Make sure it hasn't been updated on staging
    Git.cleanAndResetToStaging
    stagingDerivationContents <- readfile derivationFile
    eitherToError
      errorExit
      (Nix.oldVersionOn updateEnv "staging" stagingDerivationContents)
    Git.checkoutAtMergeBase (branchName updateEnv)

    oneHourAgo <- liftIO $ addUTCTime (fromInteger $ -60 * 60) <$> getCurrentTime

    mergeBaseOutpathsInfo <- liftIO $ readIORef mergeBaseOutpathsContext

    mergeBaseOutpathSet <-
      if lastUpdated mergeBaseOutpathsInfo < oneHourAgo
      then do
        mbos <- eitherToError errorExit currentOutpathSet
        now <- liftIO $ getCurrentTime
        liftIO $ writeIORef mergeBaseOutpathsContext (MergeBaseOutpathsInfo now mbos)
        return $ mbos
      else return $ mergeBaseOutpaths mergeBaseOutpathsInfo

    derivationContents <- readfile derivationFile
    unless (Nix.numberOfFetchers derivationContents <= 1) $
      errorExit $ "More than one fetcher in " <> toTextIgnore derivationFile
    eitherToError errorExit (pure (Blacklist.content derivationContents))
    oldHash <- eitherToError errorExit (Nix.getOldHash attrPath)
    oldSrcUrl <- eitherToError errorExit (Nix.getSrcUrl attrPath)
    File.replace (oldVersion updateEnv) (newVersion updateEnv) derivationFile
    newSrcUrl <- eitherToError errorExit (Nix.getSrcUrl attrPath)
    when (oldSrcUrl == newSrcUrl) $ errorExit "Source url did not change."
    newHash <-
      canFail (T.strip <$> cmd "nix-prefetch-url" "-A" (attrPath <> ".src")) `orElse`
      fixSrcUrl updateEnv derivationFile attrPath oldSrcUrl `orElse`
      errorExit "Could not prefetch new version URL."
    when (oldHash == newHash) $ errorExit "Hashes equal; no update necessary"
    File.replace oldHash newHash derivationFile

    editedOutpathSet <- eitherToError errorExit currentOutpathSet
    let opDiff = S.difference mergeBaseOutpathSet editedOutpathSet

    let numPRebuilds = numPackageRebuilds opDiff
    log $ "num package rebuilds: " <> (T.pack . show) numPRebuilds <> "  > 10"

    if numPRebuilds > 10 && "buildPythonPackage" `T.isInfixOf` derivationContents
      then errorExit "Package contained buildPythonPackage and too many package rebuilds"
      else return ()

    eitherToError errorExit (Nix.build attrPath)
    result <-
      fromText <$>
      (T.strip <$>
       (cmd "readlink" "./result" `orElse` cmd "readlink" "./result-bin")) `orElse`
      errorExit "Could not find result link."

    publishPackage log updateEnv oldSrcUrl newSrcUrl attrPath result opDiff

publishPackage ::
     (Text -> Sh ()) -> UpdateEnv -> Text -> Text -> Text -> FilePath -> Set ResultLine -> Sh Bool
publishPackage log updateEnv oldSrcUrl newSrcUrl attrPath result opDiff = do
  let errorExit = errorExit' log (branchName updateEnv)
  log ("cachix " <> (T.pack . show) result)
  Nix.cachix result
  resultCheckReport <-
    case Blacklist.checkResult (packageName updateEnv) of
      Right () -> sub (Check.result updateEnv result)
      Left msg -> pure msg
  d <- eitherToError errorExit (Nix.getDescription attrPath)
  let metaDescription =
        "\n\nmeta.description for " <> attrPath <> " is: '" <> d <> "'."
  releaseUrlResult <- liftIO $ GH.releaseUrl newSrcUrl
  releaseUrlMessage <-
    case releaseUrlResult of
      Left e -> do
        log e
        return ""
      Right msg -> return ("\n[Release on GitHub](" <> msg <> ")\n\n")
  compareUrlResult <- liftIO $ GH.compareUrl oldSrcUrl newSrcUrl
  compareUrlMessage <-
    case compareUrlResult of
      Left e -> do
        log e
        return "\n"
      Right msg -> return ("\n[Compare changes on GitHub](" <> msg <> ")\n\n")
  maintainers <- eitherToError errorExit (Nix.getMaintainers attrPath)
  let maintainersCc =
        if not (T.null maintainers)
          then "\n\ncc " <> maintainers <> " for testing."
          else ""
  let commitMsg = commitMessage updateEnv attrPath
  Git.commit commitMsg
  commitHash <- Git.headHash
  -- Try to push it three times
  Git.push updateEnv `orElse` Git.push updateEnv `orElse` Git.push updateEnv
  isBroken <- eitherToError errorExit (Nix.getIsBroken attrPath)
  untilOfBorgFree
  let base = if numPackageRebuilds opDiff < 100
             then "master"
             else "staging"
  GH.pr base
    (prMessage
       updateEnv
       isBroken
       metaDescription
       releaseUrlMessage
       compareUrlMessage
       resultCheckReport
       commitHash
       attrPath
       maintainersCc
       result
       (outpathReport opDiff))
  Git.cleanAndResetToMaster
  return True

repologyUrl :: UpdateEnv -> Text
repologyUrl updateEnv = [text|https://repology.org/metapackage/$pname/versions|]
  where
    pname = (packageName >>> T.toLower) updateEnv

commitMessage :: UpdateEnv -> Text -> Text
commitMessage updateEnv attrPath =
  let oV = oldVersion updateEnv
      nV = newVersion updateEnv
      repologyLink = repologyUrl updateEnv
   in [text|
       $attrPath: $oV -> $nV

       Semi-automatic update generated by
       https://github.com/ryantm/nixpkgs-update tools. This update was made
       based on information from
       $repologyLink
     |]

brokenWarning :: Bool -> Text
brokenWarning False = ""
brokenWarning True =
  "- WARNING: Package has meta.broken=true; Please manually test this package update and remove the broken attribute."

prMessage ::
     UpdateEnv
  -> Bool
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> FilePath
  -> Text
  -> Text
prMessage updateEnv isBroken metaDescription releaseUrlMessage compareUrlMessage resultCheckReport commitHash attrPath maintainersCc resultPath opReport =
  let brokenMsg = brokenWarning isBroken
      oV = oldVersion updateEnv
      nV = newVersion updateEnv
      repologyLink = repologyUrl updateEnv
      result = toTextIgnore resultPath
   in [text|
       $attrPath: $oV -> $nV

       Semi-automatic update generated by https://github.com/ryantm/nixpkgs-update tools. This update was made based on information from $repologyLink.
       $brokenMsg
       $metaDescription
       $releaseUrlMessage
       $compareUrlMessage
       <details>
       <summary>
       Checks done (click to expand)
       </summary>

       - built on NixOS
       $resultCheckReport

       </details>
       <details>
       <summary>
       Outpath report (click to expand)
       </summary>

       $opReport

       </details>

       <details>
       <summary>
       Instructions to test this update (click to expand)
       </summary>

       Either download from Cachix:
       ```
       nix-store -r $result \
         --option binary-caches 'https://cache.nixos.org/ https://r-ryantm.cachix.org/' \
         --option trusted-public-keys '
         r-ryantm.cachix.org-1:gkUbLkouDAyvBdpBX0JOdIiD2/DP1ldF3Z3Y6Gqcc4c=
         cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
         '
       ```
       (r-ryantm's Cachix cache is only trusted for this store-path realization.)

       Or, build yourself:
       ```
       nix-build -A $attrPath https://github.com/r-ryantm/nixpkgs/archive/$commitHash.tar.gz
       ```

       After you've downloaded or built it, look at the files and if there are any, run the binaries:
       ```
       ls -la $result
       ls -la $result/bin
       ```


       </details>
       <br/>
       $maintainersCc
    |]

untilOfBorgFree :: Sh ()
untilOfBorgFree = do
  waiting :: Int <-
    tRead <$>
    canFail
      (cmd "curl" "-s" "https://events.nix.ci/stats.php" -|-
       cmd "jq" ".evaluator.messages.waiting")
  when (waiting > 2) $ do
    sleep 60
    untilOfBorgFree
