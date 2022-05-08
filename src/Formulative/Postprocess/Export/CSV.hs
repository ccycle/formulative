{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Formulative.Postprocess.Export.CSV where

import Data.Csv

encodeLF x = encodeWith (defaultEncodeOptions{encUseCrLf = False}) x