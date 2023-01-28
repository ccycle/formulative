module Formulative.Postprocess.Export.CSV where

import Data.ByteString.Lazy
import Data.Csv

encodeLF :: ToRecord a => [a] -> ByteString
encodeLF = encodeWith (defaultEncodeOptions{encUseCrLf = False})