module Formulative.Calculation.Internal.Infix where

import Control.Arrow
import Control.Monad

infixl 3 <^
infixl 3 ^>

(<^) :: Functor f => f a -> (a -> b) -> f b
(<^) = flip (<$>)

(^>) :: Applicative f => f (a -> b) -> f a -> f b
(^>) = (<*>)

(<$<) :: Functor f => (b -> c) -> f (a -> b) -> f (a -> c)
(<$<) a b = (a .) <$> b
infixr 4 <$<
{-# INLINE (<$<) #-}

(|.|) :: Applicative f => f (b -> c) -> f (a -> b) -> f (a -> c)
(|.|) a b = ((.) <$> a) <*> b
infixr 9 |.|
{-# INLINE (|.|) #-}