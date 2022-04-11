{-# LANGUAGE DeriveAnyClass #-}

module Test.TH where

import Dhall
import Formulative.Calculation.Internal.TH
import Language.Haskell.TH

-- NOTE: 手元の環境ではこの関数を使うとhaskell-language-serverが壊れる
-- @ $(derivingSetting [d|data Test a = Test a a|]) @

data Test a = Test a a
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall)

-- declareTest :: Q [Dec]
-- declareTest =
--     [d|
--         data Test a = Test a a
--             deriving stock (Generic, Show, Eq)
--             deriving anyclass (FromDhall, ToDhall, Hashable)
--         |]
-- declareTest1 :: Q [Dec]
-- declareTest1 =
--     [d|
--         data Test a = Test a a
--         |]
-- declareTest2 :: Q [Dec]
-- declareTest2 = Prelude.map repplaceDeclare <$> declareTest1

{-
Q [dec] の宣言はモジュールを別に分ける必要がある
このモジュール内で $declareTest2 を宣言することはできない
-}

-- repplaceDeclare' dataD x = case dataD of
--     (DataD cxt name tyvarbndr maybekind con _) -> (DataD cxt name tyvarbndr maybekind con x)
--     _ -> dataD
-- derivingClause =
--     [ DerivClause
--         (Just StockStrategy)
--         [ConT ''GHC.Show.Show, ConT ''GHC.Classes.Eq, ConT ''GHC.Generics.Generic]
--     , DerivClause (Just AnyclassStrategy) [ConT ''Dhall.Marshal.Decode.FromDhall, ConT ''Dhall.Marshal.Encode.ToDhall]
--     ]
-- repplaceDeclare dataD = repplaceDeclare' dataD derivingClause
