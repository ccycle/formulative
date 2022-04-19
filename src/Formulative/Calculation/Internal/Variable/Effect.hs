module Formulative.Calculation.Internal.Variable.Effect where

import Control.Algebra
import Data.Kind

newtype VariableNew a = VariableNew {unVariableNew :: a}
newtype VariableOld a = VariableOld {unVariableOld :: a}

data Variable a (m :: Type -> Type) k where
    GetVariableOld :: Variable a m (VariableOld a)
    PutVariableOld :: VariableOld a -> Variable a m ()
    GetVariableNew :: Variable a m (VariableNew a)
    PutVariableNew :: VariableNew a -> Variable a m ()

getVariableNew :: (Has (Variable a) sig m) => m (a)
getVariableNew = unVariableNew <$> send GetVariableNew
putVariableNew :: forall a sig m. (Has (Variable a) sig m) => a -> m ()
putVariableNew x = send (PutVariableNew . VariableNew $ x)

getVariableOld :: (Has (Variable a) sig m) => m (a)
getVariableOld = unVariableOld <$> send GetVariableOld
putVariableOld :: forall a sig m. (Has (Variable a) sig m) => a -> m ()
putVariableOld x = send (PutVariableOld . VariableOld $ x)