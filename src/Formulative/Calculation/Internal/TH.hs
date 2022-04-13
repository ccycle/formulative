{-# LANGUAGE TemplateHaskell #-}

module Formulative.Calculation.Internal.TH (
    pprintIO,
    derivingVariable,
    derivingSetting,
    -- module Formulative.Calculation.Algebra.Arithmetic.Class,
    -- module Formulative.Calculation.VectorSpace.Class,
    module GHC.Generics,
    module Dhall.Marshal.Decode,
    module Dhall.Marshal.Encode,
    module Data.Csv,
) where

import Data.Csv hiding (Field, Name)
import Data.Hashable
import Dhall.Marshal.Decode (FromDhall)
import Dhall.Marshal.Encode (ToDhall)
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.VectorSpace.Class
import GHC.Classes
import GHC.Generics
import GHC.Show (Show (..))
import Language.Haskell.TH

pprintIO dec = runQ dec >>= putStrLn . pprint

-- deriving stock (Show, Generic)
-- derivingEqShow :: Name -> Q [Dec]
-- derivingEqShow typeName = do
--     let typeCon = conT typeName
--     [d|
--         deriving stock instance Show $(typeCon)

--         deriving stock instance Generic $(typeCon)
--         |]

-- deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace, ToRecord, ToField)
-- derivingVariable :: Name -> Q [Dec]
-- derivingVariable typeName = do
--     let typeCon = conT typeName
--     [d|
--         deriving anyclass instance Additive $(typeCon)

--         deriving anyclass instance AdditiveGroup $(typeCon)

--         deriving anyclass instance VectorSpace $(typeCon)

--         deriving anyclass instance NormSpace $(typeCon)

--         deriving anyclass instance InnerProductSpace $(typeCon)
--         |]

addDerivClause x dataD = case dataD of
    (DataD cxt name tyvarbndr maybekind con derivClause) -> DataD cxt name tyvarbndr maybekind con (derivClause <> x)
    (NewtypeD cxt name tyvarbndr maybekind con derivClause) -> NewtypeD cxt name tyvarbndr maybekind con (derivClause <> x)
    _ -> dataD
derivingClauseSetting =
    [ DerivClause
        (Just StockStrategy)
        [ConT ''Show, ConT ''Eq, ConT ''Generic]
    , DerivClause (Just AnyclassStrategy) [ConT ''FromDhall, ConT ''ToDhall, ConT ''Hashable]
    ]
derivingClauseVariable =
    [ DerivClause
        (Just StockStrategy)
        [ ConT ''Show
        , ConT ''Generic
        ]
    , DerivClause
        (Just AnyclassStrategy)
        [ ConT ''Additive
        , ConT ''AdditiveGroup
        , ConT ''VectorSpace
        , ConT ''NormSpace
        , ConT ''InnerProductSpace
        , ConT ''ToRecord
        , ConT ''ToNamedRecord
        ]
    ]

-- derivingSetting :: Q [Dec] -> Q [Dec]
derivingSetting dataD = Prelude.map (addDerivClause derivingClauseSetting) <$> dataD

{- | Funtion for automatic deriving. The following GHC extensions must be added to the header:

  @
  {\-# LANGUAGE TemplateHaskell #-\}
  {\-# LANGUAGE QuasiQuotes #-\}
  @

  For example,

  @ derivingVariable [d| data Var a = Var a a|] @

  is equivalent to:

  @
  data Var a = Var a a
     deriving stock (Show,Generics)
     deriving anyclass (Additive,AdditiveGroup,VectorSpace,NormSpace,InnerProductSpace,ToRecord,ToNamedRecord)
  @
-}
derivingVariable :: Q [Dec] -> Q [Dec]
derivingVariable dataD = Prelude.map (addDerivClause derivingClauseSetting) <$> dataD

{-
-- TODO: TemplateHaskellを使ってCarrierを自動で導出できるようにする
newtype OutputDir = OutputDir (Path Rel Dir)
    deriving stock (Generic, Show, Eq)

data OutputDirEff m k where
    GetOutputDir :: OutputDirEff m OutputDir
    LocalOutputDir :: (OutputDir -> OutputDir) -> m a -> OutputDirEff m a

getOutputDir :: (Has OutputDirEff sig m) => m OutputDir
getOutputDir = send GetOutputDir
localOutputDir :: (Has OutputDirEff sig m) => (OutputDir -> OutputDir) -> m a -> m a
localOutputDir f m = send (LocalOutputDir f m)

newtype OutputDirC m a = OutputDirC {runOutputDirC :: ReaderC OutputDir m a}
    deriving stock (Functor)
    deriving newtype (Applicative, Monad)

instance (Algebra sig m) => Algebra (OutputDirEff :+: sig) (OutputDirC m) where
    alg hdl sig ctx = case sig of
        L GetOutputDir -> do
            env <- OutputDirC (ask @OutputDir)
            pure (env <$ ctx)
        L (LocalOutputDir f m) -> do
            env <- OutputDirC (ask @OutputDir)
            y <- hdl (m <$ ctx)
            run (f env) (pure y)
          where
            run r = runReader r . runOutputDirC
        R other -> OutputDirC (alg (runOutputDirC . hdl) (R other) ctx)

-}