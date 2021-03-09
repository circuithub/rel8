{-# language BlockArguments #-}
{-# language OverloadedStrings #-}

module Main where

import Build_doctests ( flags, pkgs, module_sources )
import Control.Exception ( bracket, throwIO )
import Data.ByteString.Char8 ( unpack )
import Data.Foldable ( traverse_ )
import Hasql.Connection ( acquire, release )
import Hasql.Session ( run, sql )
import Database.Postgres.Temp ( toConnectionString, with, withConfig, verboseConfig )
import System.Environment ( setEnv )
import System.Environment.Compat ( unsetEnv )
import Test.DocTest ( doctest )

main :: IO ()
main = do
  print args
  -- unsetEnv "GHC_ENVIRONMENT" -- see 'Notes'; you may not need this
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

    doctest args
  
  where
    args = flags ++ pkgs ++ module_sources
