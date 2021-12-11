module CustomPrelude (
  module RIO.Prelude,
  module RIO.Prelude.Simple,
  module RIO.Prelude.Types,
  module RIO.List,
  IfThenElse (..),
) where

import RIO.List
import RIO.Prelude hiding (
  ask,
  asks,
  fromInteger,
  lift,
  local,
  runReader,
  runReaderT,
 )
import RIO.Prelude.Simple
import RIO.Prelude.Types hiding (Reader, ReaderT)

class IfThenElse b where
  ifThenElse :: b -> a -> a -> a

instance IfThenElse Bool where
  ifThenElse True t _ = t
  ifThenElse False _ f = f
