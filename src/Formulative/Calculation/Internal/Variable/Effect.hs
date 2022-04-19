module Formulative.Calculation.Internal.Variable.Effect where

import Control.Algebra
import Data.Kind

newtype VariableNew a = VariableNew a
newtype VariableOld a = VariableOld a

data Variable a (m :: Type -> Type) k where
    GetVariableOld :: Variable a m (VariableOld a)
    PutVariableOld :: VariableOld a -> Variable a m ()
    GetVariableNew :: Variable a m (VariableNew a)
    PutVariableNew :: VariableNew a -> Variable a m ()

getVariableNew :: (Has (Variable a) sig m) => m (VariableNew a)
getVariableNew = send GetVariableNew
putVariableNew :: forall a sig m. (Has (Variable a) sig m) => VariableNew a -> m ()
putVariableNew x = send (PutVariableNew x)

getVariableOld :: (Has (Variable a) sig m) => m (VariableOld a)
getVariableOld = send GetVariableOld
putVariableOld :: forall a sig m. (Has (Variable a) sig m) => VariableOld a -> m ()
putVariableOld x = send (PutVariableOld x)