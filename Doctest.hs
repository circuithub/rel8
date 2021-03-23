{-# language BlockArguments #-}
{-# language OverloadedStrings #-}

module Main where

import Build_doctests ( flags, pkgs, module_sources )
import Control.Exception ( bracket, throwIO )
import Data.ByteString.Char8 ( unpack )
import Data.Foldable ( traverse_ )
import Database.PostgreSQL.Simple ( connectPostgreSQL, close, execute_ )
import Database.Postgres.Temp ( toConnectionString, with, withConfig, verboseConfig )
import System.Environment ( setEnv )
import System.Environment.Compat ( unsetEnv )
import Test.DocTest ( doctest )

main :: IO ()
main = do
  unsetEnv "GHC_ENVIRONMENT" -- see 'Notes'; you may not need this
  either throwIO return =<< with \db -> do
    setEnv "TEST_DATABASE_URL" (unpack (toConnectionString db))
    bracket (connectPostgreSQL (toConnectionString db)) close \conn -> do
      execute_ conn "create table author ( author_id serial primary key, name text not null, url text )"
      execute_ conn "create table project ( author_id int not null references author (author_id), name text not null )"

      execute_ conn "insert into author ( name, url ) values ( 'Ollie', 'https://ocharles.org.uk' )"
      execute_ conn "insert into author ( name, url ) values ( 'Bryan O''Sullivan', null )"
      execute_ conn "insert into project ( author_id, name ) values ( 1, 'rel8' )"
      execute_ conn "insert into project ( author_id, name ) values ( 2, 'aeson' )"

    doctest args
  
  where
    args = flags ++ pkgs ++ module_sources
