{-# LANGUAGE OverloadedStrings, TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module Main where

import DatabaseP
import Persona
import Data.Acid
import Data.Typeable
import Data.SafeCopy
import System.CPUTime
import Text.Printf

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

diezMillones :: Int
diezMillones = 10000000

main :: IO ()
main = 
  do
    putStrLn "Starting..."
    time $
      do
        database <- openLocalStateFrom "db/" (D [])

        putStrLn "Generando diez millones de personas..."
        personas <- personasRandom diezMillones
        putStrLn "Personas generadas"

        putStrLn "Insertando diez millones de personas..."
        update database (SetPersonas personas)
        putStrLn "Personas insertadas!"

        putStrLn "Calculando el promedio de edad..."
        p <- query database PromedioEdad

        print "El promedio de edad es"
        print p
    putStrLn "Done."