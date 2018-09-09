{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module DatabaseP where

import Persona
import Data.Acid
import Data.Typeable
import Data.SafeCopy
import System.Random
import Control.Monad
import Control.Monad.Reader (ask)
import Control.Monad.State (put, get)

data DatabaseP = D { personas :: [Persona] }

getPersonas :: Query DatabaseP [Persona]
getPersonas =
  do 
    d <- ask
    return (personas d)

promedioEdad :: Query DatabaseP Int
promedioEdad =
  do
    (D ps) <- ask
    return (Persona.promedioEdad ps)

setPersonas :: [Persona] -> Update DatabaseP ()
setPersonas xs = put (D xs)

personasRandom :: Int -> IO [Persona]
personasRandom n = return (replicate n (P "Jorge" 32))
--   replicateM n $ 
--     do
--         x <- randomIO
--         return (P "Jorge" x)

$(deriveSafeCopy 0 'base ''DatabaseP)
$(makeAcidic ''DatabaseP ['getPersonas, 'DatabaseP.promedioEdad, 'setPersonas])
