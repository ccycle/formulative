{-# LANGUAGE UndecidableInstances #-}

module Test.FusedEffect where

import Control.Algebra
import Control.Applicative
import qualified Control.Carrier.Error.Church as Church
import Control.Carrier.Error.Either
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Exception.Safe
import GHC.TypeNats
import HStructure.Calculation.Algebra.Arithmetic.Class
import HStructure.Calculation.Algebra.Arithmetic.Field
import HStructure.Calculation.DifferentialEquation.Parameter
import HStructure.Calculation.Internal.Types
import HStructure.Preprocess.Exception
import Refined
import Test.Tasty

monadThrowTest :: (MonadThrow m) => m Double
monadThrowTest = do
    let a = 1
    b <- refineThrow @Dividable 2
    return $ safeDiv a b

-- 一旦Eitherで例外処理を記述
-- liftEitherで持ち上げる
-- 最後にrunErrorでエラーを定義
-- MonadThrow classであれば runError @SomeException として実行
-- MonadThrowからくるエラー型は具体的な型の情報を失うため
composeEffectTest =
    run $
        runError @RefineException $
            runReader (0.0 :: Double) $ do
                a <- ask @Double
                b <- liftEither $ refine @Dividable a
                return $ safeDiv a b

composeEffectTest2 :: MonadThrow m => m Double
composeEffectTest2 = eitherToMonadThrow composeEffectTest

-- 複数のRefineExceptionをどう扱うか
-- とりあえずrunErrorの階層を別にするとか
-- runErrorM :: MonadThrow m => Either e a -> m a
type (-&) (f :: * -> *) (g :: * -> *) h = f (g h)

-- composeEffectTest3 :: (Either RefineException -& Either RefineException) Double
composeEffectTest3 =
    run $
        runError @RefineException $
            runReader (0.0 :: Double) $ do
                a1 <- ask @Double
                b1 <- liftEither $ refine @Dividable a1
                runError @RefineException $
                    runReader (0.0 :: Double) $ do
                        a2 <- ask @Double
                        b2 <- liftEither $ refine @Dividable a2
                        return $ safeDiv a1 b1 * safeDiv a2 b2

-- nestしたEitherをひとつのMonadThrowに結合
composeEffectTest4 :: MonadThrow m => m Double
composeEffectTest4 = eitherToMonadThrow =<< eitherToMonadThrow composeEffectTest3

-- 投げる例外型がはっきりしている場合
composeEffectTest5 =
    run $
        runError @RefineException $
            runReader (0.0 :: Double) $ do
                a1 <- ask @Double
                b1 <- liftEither $ refine @Dividable a1
                runError @RefineException $
                    runReader (0.0 :: Double) $ do
                        a2 <- ask @Double
                        b2 <- liftEither $ refine @Dividable a2
                        -- return $ safeDiv a1 b1 * safeDiv a2 b2
                        runError @RefineException $
                            runReader (1.0 :: Double) $ do
                                a3 <- ask @Double
                                b3 <- liftEither $ refine @Dividable a2
                                return $ safeDiv a1 b1 * safeDiv a2 b2 * safeDiv a3 b3

-- eitherToMonadThrowでMonadThrowに変換、その後結合
-- 複数のエラー処理をしたい場合は各Eitherに対しハンドラーを記述
-- エラー処理を予めEither方式で記述して置かなければいけないのが難点か？
composeEffectTest6 :: MonadThrow m => m Double
composeEffectTest6 = eitherToMonadThrow =<< eitherToMonadThrow =<< eitherToMonadThrow composeEffectTest5

composeEffectTest7 :: MonadThrow m => m Double
composeEffectTest7 =
    eitherToMonadThrow . run $
        runError @SomeException $
            runReader (0.0 :: Double) $ do
                a1 <- ask @Double
                b1 <- liftEither (refineThrow @Dividable a1)
                return $ safeDiv a1 b1

composeEffectTest8 :: (Has (Error SomeException) sig m) => m Double
composeEffectTest8 =
    runReader 0.0 $ do
        a1 <- ask @Double
        b1 <- liftEither $ refineThrow @Dividable a1
        runReader (0.0 :: Double) $ do
            a2 <- ask @Double
            b2 <- liftEither $ refineThrow @Dividable a2
            runReader (1.0 :: Double) $ do
                a3 <- ask @Double
                b3 <- liftEither $ refineThrow @Dividable a2
                return $ safeDiv a1 b1 * safeDiv a2 b2 * safeDiv a3 b3

composeEffectTest9 = run $ runError @SomeException composeEffectTest8

composeEffectTest10 :: MonadThrow m => m Double
composeEffectTest10 = eitherToMonadThrow . run . runError @SomeException $ composeEffectTest8

-- composeEffectTest11 :: (Has (Error SomeException) sig m) => m Double
-- composeEffectTest11 = Church.runError (throwError @SomeException) pure composeEffectTest8

-- composeEffectTest12 :: (Has (Error SomeException) sig m) => m Double
-- composeEffectTest12 = runSomeException composeEffectTest8
applicativeTest1 :: (Has (Reader Double) sig m) => m Double
applicativeTest1 = do
    a <- ask @Double
    return $ a + a

-- applicativeTest2 :: (Algebra sig m, Has (Reader Double) sig m) => m Double
-- applicativeTest2 = applicativeTest1 <.+.> applicativeTest1

-- applicativeTest3 :: (Algebra sig m, Has (Reader Double) sig m) => m Double
-- applicativeTest3 = applicativeTest2 <.+.> applicativeTest2 <.-.> applicativeTest2 <.*.> applicativeTest2

applicativeTest4 :: (Algebra sig m, Has (Reader Double) sig m) => m (Double -> Double)
applicativeTest4 = do
    a <- applicativeTest1
    return $ \x -> a .+. x

readerTest1 :: (Algebra sig m, Has (Reader (StepSize Double)) sig m) => m Double
readerTest1 = do
    (MkStepSize t) <- ask
    return $ t + 1

runReaderTest1 = run . runReader (0.1 :: Double) . runReader (MkStepSize (0.3 :: Double)) $ readerTest1

stateTest1 :: (Algebra sig m, Member (State Double) sig) => m Double
stateTest1 = do
    a <- get
    put $ a + 1
    return a

runStateTest1 = run . runState (0.1 :: Double) $ stateTest1