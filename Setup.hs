module Main where

-- cabal-doctest
import Distribution.Extra.Doctest ( defaultMainWithDoctests )


main :: IO ()
main = defaultMainWithDoctests "doctests"
