module Formulative.Postprocess.Export.IO where

import Control.Algebra
import Control.Carrier.Error.Church
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Exception.Safe
import Control.Monad
import Data.Time
import Dhall
import Formulative.Calculation.Internal.Class
import Formulative.Postprocess.Export.Effect
import Formulative.Postprocess.Export.Types
import Formulative.Preprocess.CommandLineOptions
import Formulative.Preprocess.SettingFile.Effect
import Path
import Path.IO

putStrLnM :: (Algebra sig m, Member (Lift IO) sig) => String -> m ()
putStrLnM = sendIO . putStrLn

ensureDirOutputM :: (Algebra sig m, Member Export sig, Member (Lift IO) sig) => m ()
ensureDirOutputM = do
    (OutputDir relDir) <- askOutputDir
    sendIO . ensureDir $ relDir

doesDirExistsOutputM :: (Algebra sig m, Member Export sig, Member (Lift IO) sig) => m Bool
doesDirExistsOutputM = do
    (OutputDir relDir) <- askOutputDir
    sendIO . doesDirExist $ relDir

removeDirRecurOutputM :: (Algebra sig m, Member (Lift IO) sig) => OutputDir -> m ()
removeDirRecurOutputM (OutputDir relDir) =
    sendIO . removeDirRecur $ relDir

msgDirAlreadyExists :: (Monad m, Member (Lift IO) sig, Member (Lift IO) sig, Algebra sig m, Member Export sig) => m ()
msgDirAlreadyExists = do
    dir <- askOutputDirAbsPath
    putStrLnM $ "[WARNING] The output directory (" <> toFilePath dir <> ") already exists; the calculation may have already been executed."

-- TODO: logger作成
warningForOverwrite :: (Has (Lift IO) sig m, Member (Throw SomeException) sig, Member Export sig) => m RecalculationOption
warningForOverwrite = do
    msgDirAlreadyExists
    putStrLnM ""
    CmdOptions{..} <- sendIO cmdOptionIO
    if warningFlag
        then return Overwrite
        else do
            putStrLnM "Overwrite ([y]/n)?"
            f 5
  where
    f i =
        if i == 0
            then liftEither $ throwString "Invalid input: stop process."
            else do
                str <- sendIO getLine
                case str of
                    "" -> return Overwrite
                    "y" -> return Overwrite
                    "n" -> return NoOperation
                    _ -> do
                        putStrLnM ""
                        putStrLnM $ "Invalid input: " <> str
                        putStrLnM "RETURN -> Exec and Overwrite"
                        putStrLnM "\'y\', RETURN -> Exec and Overwrite"
                        putStrLnM "\'n\', RETURN -> Exit"
                        f (pred i)

removeDirRecurWithWarningM :: (Algebra sig m, Member (Lift IO) sig, Member Export sig, Member (Throw SomeException) sig) => m ()
removeDirRecurWithWarningM = do
    (OutputDir x) <- askOutputDir
    d <- sendIO $ doesDirExist x
    when d $ do
        r <- warningForOverwrite
        case r of
            Overwrite -> removeDirRecurOutputM (OutputDir x)
            NoOperation -> liftEither $ throwString "Exit."

parentDirM :: (Algebra sig m, Member (Lift IO) sig, Member (Throw SomeException) sig) => m (Path Rel Dir)
parentDirM = do
    t <- sendIO getZonedTime
    let dir = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" t
    liftEither $ parseRelDir $ "./output/" <> dir

-- TODO: adaptive step sizeの実装

msgNewLine :: (Has (Lift IO) sig m) => m ()
msgNewLine = putStrLnM ""

msgStart :: (Has (Lift IO) sig m) => m ()
msgStart = msgNewLine >> putStrLnM "--- Start ---"

msgDone :: (Has (Lift IO) sig m) => m ()
msgDone = putStrLnM "Done." >> msgNewLine

msgEnd :: (Has (Lift IO) sig m) => m ()
msgEnd = msgNewLine >> putStrLnM "--- End ---" >> msgNewLine

msgExportFileIO :: Path b File -> IO ()
msgExportFileIO path = putStrLn $ concat ["Exporting ", toFilePath path, " .."]

msgExportFileM :: (Has (Lift IO) sig m) => Path b File -> m ()
msgExportFileM path = sendIO $ msgExportFileIO path

msgOutputDir :: (Member Export sig, Member (Lift IO) sig, Algebra sig m) => m ()
msgOutputDir = do
    (OutputDir outputPath) <- askOutputDir
    absOutputDir <- sendIO $ makeAbsolute outputPath
    putStrLnM $ "output directory: " <> toFilePath absOutputDir
    msgNewLine

askOutputDirAbsPath :: (Member Export sig, Algebra sig m, Member (Lift IO) sig) => m (Path Abs Dir)
askOutputDirAbsPath = do
    (OutputDir outputPath) <- askOutputDir
    sendIO $ makeAbsolute outputPath

-- class MainCalcPDEManifold m (s :: Nat -> [Nat] -> *) where
--     mainCalcPDEManifold :: (KnownNat n, SingI l) => s n l -> m ()
--     initialStatePDEManifold :: (KnownNat n, SingI l) => m (s n l)

-- class MainCalcPDESubmanifold m (s :: EucDim -> Dim -> [Nat] -> *) where
--     mainCalcPDESubmanifold :: (KnownNat nEuc, KnownNat n, SingI l) => s nEuc n l -> m ()
--     initialStatePDESubmanifold :: (KnownNat nEuc, KnownNat n, SingI (l :: [Nat])) => m (s nEuc n l)

-- mainCalcPDEproxy ::
--     forall s nEuc n (l :: [Nat]) sig m.
--     ( MainCalcPDESubmanifold m s
--     , KnownNat nEuc
--     , KnownNat n
--     , SingI l
--     , Monad m
--     ) =>
--     Proxy nEuc ->
--     Proxy n ->
--     SList l ->
--     m ()
-- mainCalcPDEproxy _ _ _ = do
--     x <- (initialStatePDESubmanifold @m @s @nEuc @n @l)
--     mainCalcPDESubmanifold @m @s @nEuc @n @l x

-- runMainCalcPDE
--     (DimensionOfEuclideanSpace nEuc)
--     (DimensionOfManifold n)
--     (l :: [Natural]) =
--         case (someNatVal nEuc, someNatVal n, someSingVal l) of
--             (SomeNat (pnEuc :: Proxy nEuc'), SomeNat (pn :: Proxy n'), SomeSingI (sl :: Sing l')) ->
--                 mainCalcPDEproxy pnEuc pn sl

-- preprocessM ::
--     forall m a sig.
--     ( Algebra sig m
--     , Member (Throw SomeException) sig
--     , Member (Lift IO) sig
--     , Member Export sig
--     , Member SettingFile sig
--     , HasDependentParameterM m a
--     , ToDhall (DependentParameterType a)
--     ) =>
--     m ()
-- preprocessM = do
--     removeDirRecurWithWarningM
--     ensureDirOutputM
--     exportSettingFile
--     exportDependentParameterFile @a @m
