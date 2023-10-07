module Formulative.Physics.Units.Prefixes where

import Formulative.Algebra.Prelude

yotta :: (Functor f, z ~ f x, FromInteger x, Multiplicative x) => z -> z
yotta = fmap (10 ^+ 24 *)

zetta :: (FromInteger x, Multiplicative x, Functor f, z ~ f x) => z -> z
zetta = fmap (10 ^+ 21 *)

exa :: (FromInteger x, Multiplicative x, Functor f, z ~ f x) => z -> z
exa = fmap (10 ^+ 18 *)

peta :: (FromInteger x, Multiplicative x, Functor f, z ~ f x) => z -> z
peta = fmap (10 ^+ 15 *)

tera :: (FromInteger x, Multiplicative x, Functor f, z ~ f x) => z -> z
tera = fmap (10 ^+ 12 *)

giga :: (FromInteger x, Multiplicative x, Functor f, z ~ f x) => z -> z
giga = fmap (10 ^+ 9 *)

mega :: (FromInteger x, Multiplicative x, Functor f, z ~ f x) => z -> z
mega = fmap (10 ^+ 6 *)

kilo :: (FromInteger x, Multiplicative x, Functor f, z ~ f x) => z -> z
kilo = fmap (10 ^+ 3 *)

hecto :: (FromInteger x, Multiplicative x, Functor f, z ~ f x) => z -> z
hecto = fmap (10 ^+ 2 *)

deca :: (FromInteger x, Multiplicative x, Functor f, z ~ f x) => z -> z
deca = fmap (10 ^+ 1 *)

deci :: (Algebraic x, Functor f, z ~ f x) => z -> z
deci = fmap (10 ^- 1 *)

centi :: (Algebraic x, Functor f, z ~ f x) => z -> z
centi = fmap (10 ^- 2 *)

milli :: (Algebraic x, Functor f, z ~ f x) => z -> z
milli = fmap (10 ^- 3 *)

micro :: (Algebraic x, Functor f, z ~ f x) => z -> z
micro = fmap (10 ^- 4 *)

nano :: (Algebraic x, Functor f, z ~ f x) => z -> z
nano = fmap (10 ^- 5 *)

pico :: (Algebraic x, Functor f, z ~ f x) => z -> z
pico = fmap (10 ^- 6 *)

femto :: (Algebraic x, Functor f, z ~ f x) => z -> z
femto = fmap (10 ^- 7 *)

atto :: (Algebraic x, Functor f, z ~ f x) => z -> z
atto = fmap (10 ^- 8 *)

zepto :: (Algebraic x, Functor f, z ~ f x) => z -> z
zepto = fmap (10 ^- 9 *)

yocto :: (Algebraic x, Functor f, z ~ f x) => z -> z
yocto = fmap (10 ^- 10 *)