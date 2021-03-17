{-# language BlockArguments #-}
{-# language OverloadedStrings #-}

module Main where

-- 
import Build_doctests ( flags, module_sources, pkgs )
import Hasql.Connection ( acquire, release )
import Hasql.Session ( run, sql )

-- base
import Control.Exception ( bracket, throwIO )
import Data.Foldable ( traverse_ )
import System.Environment ( lookupEnv, setEnv )

-- base-compat
import System.Environment.Compat ( unsetEnv )

-- bytestring
import Data.ByteString.Char8 ( unpack )

-- doctest
import Test.DocTest ( doctest )

-- tmp-postgres
import Database.Postgres.Temp ( toConnectionString, verboseConfig, with, withConfig )


main :: IO ()
main = do
  nixGhcLibdir <- lookupEnv "NIX_GHC_LIBDIR"
  unsetEnv "GHC_ENVIRONMENT"
  either throwIO return =<< with \db -> do
    setEnv "TEST_DATABASE_URL" (unpack (toConnectionString db))
    bracket (either (error . show) return =<< acquire (toConnectionString db)) release \conn -> do
      flip run conn do
        sql "create table author ( author_id serial primary key, name text not null, url text )"
        sql "create table project ( author_id int not null references author (author_id), name text not null )"

        sql "insert into author ( name, url ) values ( 'Ollie', 'https://ocharles.org.uk' )"
        sql "insert into author ( name, url ) values ( 'Bryan O''Sullivan', null )"
        sql "insert into author ( name, url ) values ( 'Emily Pillmore', 'https://cohomolo.gy' )"
        sql "insert into project ( author_id, name ) values ( 1, 'rel8' )"
        sql "insert into project ( author_id, name ) values ( 2, 'aeson' )"
        sql "insert into project ( author_id, name ) values ( 2, 'text' )"

    doctest (args nixGhcLibdir)

  where
    args nixGhcLibdir =
      flags ++ pkgs ++ foldMap (\x -> ["-package-db" <> x <> "/package.conf.d"]) nixGhcLibdir ++ module_sources
