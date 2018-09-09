{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module Persona where 

import Data.Acid
import Data.Typeable
import Data.SafeCopy
import Data.List (foldl')

data Persona = P { nombre :: String, edad :: Int } deriving Show

sum' :: [Persona] -> Int
sum' = foldl' (\r (P _ n) -> r + n) 0

len' :: [Persona] -> Int
len' = foldl' (\r x -> r + 1) 0

promedioEdad :: [Persona] -> Int
promedioEdad ps =
    let s = sum' ps
        l = len' ps
        in div s l

$(deriveSafeCopy 0 'base ''Persona)
