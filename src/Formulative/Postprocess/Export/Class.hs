{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Postprocess.Export.Class where

import Control.Algebra
import Control.Carrier.Error.Either
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Exception.Safe
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import Data.Csv hiding (Field)
import qualified Data.Matrix.Static.Generic as MSG
import qualified Data.Matrix.Static.Sparse as MSS
import Data.Proxy
import Data.Singletons (Sing (..), SingI, SingInstance (SingInstance), SingKind (toSing), SomeSing (SomeSing), singInstance)
import Data.Singletons.Prelude (SList)
import qualified Data.Text.IO as T
import qualified Data.Vector.Storable as VST
import Dhall
import GHC.TypeNats
import Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Types
import Formulative.Calculation.DiscreteExteriorCalculus.Geometry.Types
import Formulative.Calculation.DiscreteExteriorCalculus.Homology.Types
import Formulative.Calculation.Internal.Class
import Formulative.Calculation.Internal.Singletons (SomeSingI (SomeSingI), someSingVal, withSomeSingI)
import Formulative.Calculation.VectorSpace.VectorSpace (Scalar)
import Formulative.Postprocess.Export.Effect
import Formulative.Postprocess.Export.Types
import Formulative.Preprocess.CommandLineOptions
import Formulative.Preprocess.ReadSetting
import Formulative.Preprocess.SettingFile.Effect (SettingFile, askSettingFileText)
import Path
import Path.IO
import RIO.Time

putStrLnM :: (Algebra sig m, Member (Lift IO) sig) => String -> m ()
putStrLnM = sendIO . putStrLn

-- TODO: (Int,Int,a)の形で出力できるようにする
-- instance (ToField a, Storable a, MSS.Zero a) => ToField (MSL.SparseMatrix m n a) where
--     toField x = toStrict . encode $ (Prelude.map (: []) . MSG.toList $ x)

-- TODO: toStrictを使わない形に直す
instance (ToField a, VST.Storable a, MSS.Zero a) => ToField (DECrepresentationMatrix n l c1 k1 c2 k2 a) where
    toField (DECrepresentationMatrix mat) = BSL.toStrict . encode $ Prelude.map (: []) . MSG.toList $ mat

-- configで設定できるようにする
-- 相対パスの指定かautoか
-- 過去の計算結果をもとに再計算させるなどの使い方を想定
-- newtype OutputDir = MkOutputDir (Path Rel Dir)
--     deriving stock (Generic, Show, Eq)
-- exportSettingFile :: m ()
exportSettingFile :: forall m sig. (Algebra sig m, Member Export sig, Member SettingFile sig, Member (Lift IO) sig) => m ()
exportSettingFile = do
    (MkOutputDir outputDir) <- askOutputDir
    (DhallSettingText x) <- askSettingFileText
    sendIO $ putStrLn "Export setting file.."
    sendIO $ ensureDir outputDir
    sName <- sendIO $ parseRelFile "setting.dhall"
    sendIO $ T.writeFile (toFilePath (outputDir </> sName)) x
    sendIO $ putStrLn "Done."

exportDependentParameterFile ::
    forall a m sig.
    ( Algebra sig m
    , Member Export sig
    , Member (Lift IO) sig
    , HasDependentParameterM m a
    , ToDhall (DependentParameterType a)
    ) =>
    m ()
exportDependentParameterFile = do
    x <- dependentParameterM @m @a
    let y = toDhallText x
    when (y /= toDhallText ()) $ do
        (MkOutputDir outputDir) <- askOutputDir
        sendIO $ putStrLn "Export dependent parameter file.."
        sendIO $ ensureDir outputDir
        sName <- sendIO $ parseRelFile "dependentParamater.dhall"
        sendIO $ T.writeFile (toFilePath (outputDir </> sName)) y
        sendIO $ putStrLn "Done."

-- exportDhallSettingTextFile :: (Algebra sig m, Member (Lift IO) sig, Member SettingFile sig,
--  Member Export sig) => m ()
-- exportDhallSettingTextFile = do
--     (MkOutputDir outputDir) <- askOutputDir
--     (DhallSettingText x) <- askSettingFileText
--     sendIO $ putStrLn "Export setting file.."
--     sendIO $ ensureDir outputDir
--     sName <- sendIO $ parseRelFile "setting.dhall"
--     sendIO $ T.writeFile (toFilePath (outputDir </> sName)) x
--     sendIO $ putStrLn "Done."

-- 1ファイルに追記
exportDependentVariableGlobal ::
    forall a sig m.
    ( Algebra sig m
    , DefaultOrdered (DependentVariableGlobalType a)
    , HasDependentVariableGlobalM m a
    , Member (Lift IO) sig
    , Member (Throw SomeException) sig
    , Member Export sig
    , ToNamedRecord (DependentVariableGlobalType a)
    , ToRecord (DependentVariableGlobalType a)
    ) =>
    a ->
    m ()
exportDependentVariableGlobal x = do
    x' <- dependentVariableGlobalM x
    (MkOutputDir parentDir) <- askOutputDir
    ensureDirOutputM
    parseKey <- liftEither $ parseRelFile "dependentVariableGlobal"
    -- TODO: CSV以外にもに対応させる
    fileName <- liftEither $ replaceExtension ".csv" parseKey
    let filePath = parentDir </> fileName
    flag <- sendIO $ doesFileExist filePath
    if flag
        then do
            -- fileがすでに存在したときは末尾に付け足す
            let str = encode [x']
            sendIO $ BSL.appendFile (toFilePath filePath) str
        else do
            -- 存在しないときは新規に作成
            let str = encodeDefaultOrderedByName [x']
            sendIO $ BSL.writeFile (toFilePath filePath) str

ensureDirOutputM :: (Algebra sig m, Member Export sig, Member (Lift IO) sig) => m ()
ensureDirOutputM = do
    (MkOutputDir relDir) <- askOutputDir
    sendIO . ensureDir $ relDir

doesDirExistsOutputM :: (Algebra sig m, Member Export sig, Member (Lift IO) sig) => m Bool
doesDirExistsOutputM = do
    (MkOutputDir relDir) <- askOutputDir
    sendIO . doesDirExist $ relDir

removeDirRecurOutputM :: (Algebra sig m, Member (Lift IO) sig) => OutputDir -> m ()
removeDirRecurOutputM (MkOutputDir relDir) =
    sendIO . removeDirRecur $ relDir

-- TODO: logger作成
warningForOverwrite :: (Has (Lift IO) sig m, Member (Throw SomeException) sig) => OutputDir -> m RecalculationOption
warningForOverwrite (MkOutputDir dir) = do
    sendIO $ putStrLn $ "[WARNING] The output directory (" <> toFilePath dir <> ") already exists: The calculation may have already been executed."
    sendIO $ putStrLn ""
    sendIO $ putStrLn "Overwrite ([y]/n)?"
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
                        sendIO $ putStrLn ""
                        sendIO $ putStrLn $ "Invalid input: " <> str
                        sendIO $ putStrLn "RETURN -> Exec and Overwrite"
                        sendIO $ putStrLn "\'y\', RETURN -> Exec and Overwrite"
                        sendIO $ putStrLn "\'n\', RETURN -> Exit"
                        f (pred i)

removeDirRecurWithWarningM :: (Algebra sig m, Member (Lift IO) sig, Member Export sig, Member (Throw SomeException) sig) => m ()
removeDirRecurWithWarningM = do
    (MkOutputDir x) <- askOutputDir
    d <- sendIO $ doesDirExist x
    when d $ do
        r <- warningForOverwrite (MkOutputDir x)
        case r of
            Overwrite -> removeDirRecurOutputM (MkOutputDir x)
            NoOperation -> liftEither $ throwString "Exit."

parentDirM :: (Algebra sig m, Member (Lift IO) sig, Member (Throw SomeException) sig) => m (Path Rel Dir)
parentDirM = do
    t <- sendIO getZonedTime
    let dir = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" t
    liftEither $ parseRelDir $ "./output/" <> dir

-- newtype IndexOfStep = MkIndexOfStep Natural
--     deriving stock (Generic, Show, Eq)
--     deriving anyclass (FromDhall, ToDhall, Hashable)
-- newtype Parameter a = MkParameter a
--     deriving stock (Generic, Show, Eq)
--     deriving anyclass (FromDhall, ToDhall, Hashable)

-- TODO: adaptive step sizeの実装
-- adaptiveStepSize (IsAdaptiveStepSize True) (MkStepSize dt) = MkStepSize dt
-- adaptiveStepSize (IsAdaptiveStepSize False) (MkStepSize dt) = MkStepSize dt

-- indexはexportのパスでも使う可能性があるので state を使う
-- mainCalculationDynamic :: (Algebra sig m, HasUpdateM m t, Ord a1, Show a1, ExportSubDataM m t,
--  Member (Lift IO) sig, Member (Reader a2) sig,
--  Member (Reader (DynamicParameterSetting a1)) sig,
--  Member (Export) sig, Member (State (Parameter a1)) sig,
--  Member (State (Variable t)) sig, Member (State (StepSize a1)) sig,
--  Member (State IndexOfStep) sig, Additive a1) =>
--  t -> m ()

msgDone :: (Has (Lift IO) sig m) => m ()
msgDone = putStrLnM "Done."

msgStart :: (Has (Lift IO) sig m) => m ()
msgStart = putStrLnM "Start."

msgEnd :: (Has (Lift IO) sig m) => m ()
msgEnd = putStrLnM "End."

class MainCalcPDEManifold m (s :: Nat -> [Nat] -> *) where
    mainCalcPDEManifold :: (KnownNat n, SingI l) => s n l -> m ()
    initialStatePDEManifold :: (KnownNat n, SingI l) => m (s n l)

class MainCalcPDESubmanifold m (s :: EucDim -> Dim -> [Nat] -> *) where
    mainCalcPDESubmanifold :: (KnownNat nEuc, KnownNat n, SingI l) => s nEuc n l -> m ()
    initialStatePDESubmanifold :: (KnownNat nEuc, KnownNat n, SingI (l :: [Nat])) => m (s nEuc n l)

mainCalcPDEproxy ::
    forall s nEuc n (l :: [Nat]) sig m.
    ( MainCalcPDESubmanifold m s
    , KnownNat nEuc
    , KnownNat n
    , SingI l
    , Monad m
    ) =>
    Proxy nEuc ->
    Proxy n ->
    SList l ->
    m ()
mainCalcPDEproxy _ _ _ = do
    x <- (initialStatePDESubmanifold @m @s @nEuc @n @l)
    mainCalcPDESubmanifold @m @s @nEuc @n @l x

runMainCalcPDE
    (MkDimensionOfEuclideanSpace nEuc)
    (MkDimensionOfManifold n)
    (l :: [Natural]) =
        case (someNatVal nEuc, someNatVal n, someSingVal l) of
            (SomeNat (pnEuc :: Proxy nEuc'), SomeNat (pn :: Proxy n'), SomeSingI (sl :: Sing l')) ->
                mainCalcPDEproxy pnEuc pn sl

preprocessM :: forall m a sig. (Algebra sig m, Member (Throw SomeException) sig, Member (Lift IO) sig, Member Export sig, Member SettingFile sig, HasDependentParameterM m a, ToDhall (DependentParameterType a)) => m ()
preprocessM = do
    removeDirRecurWithWarningM
    ensureDirOutputM
    exportSettingFile
    exportDependentParameterFile @a @m