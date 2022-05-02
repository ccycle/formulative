{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.FusedEffect where

import Control.Algebra
import Control.Applicative
import qualified Control.Carrier.Error.Church as Church
import Control.Carrier.Error.Either
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Data.Kind
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Algebra.Arithmetic.Field
import Formulative.Calculation.DifferentialEquation.Types
import Formulative.Calculation.Internal.Types
import Formulative.Preprocess.Exception
import GHC.TypeNats

-- import Formulative.Preprocess.Label

import Formulative.Postprocess.Export.IO
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
    (StepSize t) <- ask
    return $ t + 1

runReaderTest1 = run . runReader (0.1 :: Double) . runReader (StepSize (0.3 :: Double)) $ readerTest1

stateTest1 :: (Algebra sig m, Member (State Double) sig) => m Double
stateTest1 = do
    a <- get
    put $ a + 1
    return a

-- stateTest2 :: (Algebra sig m, Member (State Double) sig) => m [Double]
-- stateTest2 = do
--     a <- get
--     put a
--     return [a]

runStateTest1 = run . runState (0.1 :: Double) $ stateTest1
runStateTest2 = run . evalState (0.1 :: Double) $ stateTest1
runStateTest3 = run . execState (0.1 :: Double) $ stateTest1

printParameterNameTest :: (Member (Lift IO) sig, Algebra sig m, Member (Reader LabelOfDynamicParameter) sig) => m ()
printParameterNameTest = do
    LabelOfDynamicParameter name <- ask
    sendIO $ putStrLn ("label name: " <> name)

-- instance HasParameterName sig m where
--     getParameterNameM =

runTest f = do
    sendIO $ putStr "input str: "
    str <- sendIO getLine
    runReader (LabelOfDynamicParameter str) f

runTestParameterName :: IO ()
runTestParameterName = runM . runReader (LabelOfDynamicParameter "test") $ printParameterNameTest

runTestParameterName2 :: IO ()
runTestParameterName2 = runM . runTest $ printParameterNameTest

data Teletype (m :: Type -> Type) k where
    Read :: Teletype m String
    Write :: String -> Teletype m ()

read :: Has Teletype sig m => m String
read = send Read

write :: Has Teletype sig m => String -> m ()
write s = send (Write s)

newtype TeletypeIOC m a = TeletypeIOC {runTeletypeIO :: m a}
    deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (Teletype :+: sig) (TeletypeIOC m) where
    alg hdl sig ctx = case sig of
        L Read -> (<$ ctx) <$> liftIO getLine
        L (Write s) -> ctx <$ liftIO (putStrLn s)
        R other -> TeletypeIOC (alg (runTeletypeIO . hdl) other ctx)

-- 1つのeffectに対し複数のCarrierを設定する
-- label config test
data EnvTest a = EnvTest {getLabelsTest :: [String], getStepSizeTest :: StepSize a}
defaultEnvTest = EnvTest{getLabelsTest = ["LabelTest1", "LabelTest2"], getStepSizeTest = StepSize 0.1}

data EnvEff a (m :: Type -> Type) k where
    GetEnv :: EnvEff a m (EnvTest a)
getEnv :: Has (EnvEff a) sig m => m (EnvTest a)
getEnv = send GetEnv

-- newtype EnvC m a = EnvC (m (EnvTest a))
-- newtype LabelsC m a = ReaderC [String] m a
newtype LabelsC m a = LabelsC {runLabels :: ReaderC [String] m a}
    deriving stock (Functor)
    deriving newtype (Applicative, Monad)
newtype LabelsEnvC a m b = LabelsEnvC {runLabelsEnv :: ReaderC (EnvTest a) m b}
    deriving stock (Functor)
    deriving newtype (Applicative, Monad)

-- instance Applicative m => Applicative (LabelsC m) where
--     pure = LabelsC . const . pure
--     LabelsC f <*> LabelsC a = LabelsC (liftA2 (<*>) f a)
--     {-# INLINE (<*>) #-}
-- instance Monad m => Monad (LabelsC m) where
--     LabelsC a >>= f = LabelsC (\r -> a r >>= runLabelsC r . f)

--   {-# INLINE (>>=) #-}
data LabelsEff (m :: Type -> Type) k where
    GetLabels :: LabelsEff m [String]
getLabels :: Has LabelsEff sig m => m [String]
getLabels = send GetLabels

instance (Algebra sig m) => Algebra (LabelsEff :+: sig) (LabelsC m) where
    alg hdl sig ctx = case sig of
        L GetLabels -> do
            l <- LabelsC (ask @[String])
            pure ((<$ ctx) l)
        R other -> LabelsC (alg (runLabels . hdl) (R other) ctx)

instance (Algebra sig m) => Algebra (LabelsEff :+: sig) (LabelsEnvC a m) where
    alg hdl sig ctx = case sig of
        L GetLabels -> do
            l <- getLabelsTest <$> LabelsEnvC (ask @(EnvTest a))
            pure ((<$ ctx) l)
        R other -> LabelsEnvC (alg (runLabelsEnv . hdl) (R other) ctx)

printLabelsTest :: (Has LabelsEff sig m, Member (Lift IO) sig) => m ()
printLabelsTest = do
    l <- getLabels
    forM_ l putStrLnM

-- runLabelsTest :: IO ()
-- runLabelsTest :: (Algebra sig m, Member (Lift IO) sig, Member LabelsEff sig) => m ()
-- runLabelsTest :: m ()
runLabelsTest1 = runReader ["LabelTest1", "LabelTest2"] . runLabels

runLabelsTest2 :: forall m a. LabelsEnvC Double m a -> m a
runLabelsTest2 = runReader defaultEnvTest . runLabelsEnv

actionTest1 :: IO ()
actionTest1 = runM . runLabelsTest1 $ printLabelsTest

actionTest2 :: IO ()
actionTest2 = runM . runLabelsTest2 $ printLabelsTest

-- step size
data StepSizeEff a (m :: Type -> Type) k where
    GetStepSize :: StepSizeEff a m (StepSize a)
getStepSize :: forall a sig m. Has (StepSizeEff a) sig m => m (StepSize a)
getStepSize = send GetStepSize

-- carrier
newtype StepSizeC a m b = StepSizeC {runStepSize :: ReaderC (StepSize a) m b}
    deriving stock (Functor)
    deriving newtype (Applicative, Monad)
newtype StepSizeEnvC a m b = StepSizeEnvC {runStepSizeEnv :: ReaderC (EnvTest a) m b}
    deriving stock (Functor)
    deriving newtype (Applicative, Monad)

instance (Algebra sig m) => Algebra (StepSizeEff a :+: sig) (StepSizeC a m) where
    alg hdl sig ctx = case sig of
        L GetStepSize -> do
            env <- StepSizeC (ask @(StepSize a))
            -- let l = getStepSizeTest env
            pure ((<$ ctx) env)
        R other -> StepSizeC (alg (runStepSize . hdl) (R other) ctx)
instance (Algebra sig m) => Algebra (StepSizeEff a :+: sig) (StepSizeEnvC a m) where
    alg hdl sig ctx = case sig of
        L GetStepSize -> do
            env <- StepSizeEnvC (ask @(EnvTest a))
            let l = getStepSizeTest env
            pure ((<$ ctx) l)
        R other -> StepSizeEnvC (alg (runStepSizeEnv . hdl) (R other) ctx)

-- 1つのEffectにたいしCarrierを複数定めることはできる
-- 逆はできない
-- instance (Algebra sig m) => Algebra (StepSizeEff a :+: sig) (ReaderC (StepSize a) m) where
--     alg hdl sig ctx = case sig of
--         L GetStepSize -> do
--             env <- StepSizeEnvC (ask @(EnvTest a))
--             let l = getStepSizeTest env
--             pure ((<$ ctx) l)
--         R other -> StepSizeEnvC (alg (runStepSizeEnv . hdl) (R other) ctx)

runStepSizeTest1 :: forall m a. StepSizeEnvC Double m a -> m a
runStepSizeTest1 = runReader defaultEnvTest . runStepSizeEnv

printStepSizeTest :: forall a sig m. (Show a, Has (StepSizeEff a) sig m, Member (Lift IO) sig) => m ()
printStepSizeTest = do
    l <- getStepSize @a
    sendIO $ print l

actionTestStepSize :: IO ()
actionTestStepSize = runM . runStepSizeTest1 $ (printStepSizeTest @Double)

printEff :: forall a0 sig0 m. (Algebra sig0 m, Show a0, Member (StepSizeEff a0) sig0, Member LabelsEff sig0, Algebra sig0 m, Member (Lift IO) sig0) => m ()
printEff = do
    l <- getStepSize @a0
    x <- getLabels
    sendIO $ putStrLn $ concat [show l, " ", show x]

actionComposedEff1 :: IO ()
actionComposedEff1 = runM . runLabelsTest2 . runStepSizeTest1 $ printEff @Double

actionComposedEff2 :: IO ()
actionComposedEff2 = runM . runStepSizeTest1 . runLabelsTest2 $ printEff @Double