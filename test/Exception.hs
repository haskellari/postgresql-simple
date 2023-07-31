{-# LANGUAGE  CPP #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Exception (testExceptions) where

import Database.PostgreSQL.Simple
import Test.Tasty.HUnit (Assertion, assertBool)
import Common (TestEnv)
import Control.Exception (Exception (..), SomeException)
import Data.Maybe (isJust)
import Data.Either (isLeft)
import Control.Exception (throwIO, try)

testExceptions :: TestEnv -> Assertion
testExceptions _ = do
  let sqlError = SqlError
        { sqlState       = ""
        , sqlExecStatus  = FatalError
        , sqlErrorMsg    = ""
        , sqlErrorDetail = ""
        , sqlErrorHint   = ""
        }
  let sqlEx :: SomeException = toException sqlError
  assertBool "SqlError is SomePostgreSqlException" $ isJust (fromException sqlEx :: Maybe SomePostgreSqlException)
  assertBool "SqlError is SqlError" $ isJust (fromException sqlEx :: Maybe SqlError)
  eSqlError :: Either SqlError () <- try $ throwIO sqlEx
  assertBool "Can catch SqlError" $ isLeft eSqlError
  eSqlPostgreSqlEx :: Either SomePostgreSqlException () <- try $ throwIO sqlEx
  assertBool "Can catch SomePostgreSqlException from SqlError" $ isLeft eSqlPostgreSqlEx

  let formatError = FormatError
        { fmtMessage = ""
        , fmtQuery = ""
        , fmtParams = []
        }
  let formatEx :: SomeException = toException formatError
  assertBool "FormatError is SomePostgreSqlException" $ isJust (fromException formatEx :: Maybe SomePostgreSqlException)
  assertBool "FormatError is FormatError" $ isJust (fromException formatEx :: Maybe FormatError)
  eFormatError :: Either FormatError () <- try $ throwIO formatEx
  assertBool "Can catch FormatError" $ isLeft eFormatError
  eFormatPostreSqlEx :: Either SomePostgreSqlException () <- try $ throwIO formatEx
  assertBool "Can catch SomePostgreSqlException from FormatError" $ isLeft eFormatPostreSqlEx

  let queryError = QueryError
          { qeMessage = ""
          , qeQuery = ""
          }
  let queryEx :: SomeException = toException queryError
  assertBool "QueryError is SomePostgreSqlException" $ isJust (fromException queryEx :: Maybe SomePostgreSqlException)
  assertBool "QueryError is QueryError" $ isJust (fromException queryEx :: Maybe QueryError)
  eQueryError :: Either QueryError () <- try $ throwIO queryEx
  assertBool "Can catch QueryError" $ isLeft eQueryError
  eQueryPostgreSqlEx :: Either SomePostgreSqlException () <- try $ throwIO queryEx
  assertBool "Can catch SomePostgreSqlException from QueryError" $ isLeft eQueryPostgreSqlEx

  let resultError = Incompatible
        { errSQLType = ""
        , errSQLTableOid = Nothing
        , errSQLField = ""
        , errHaskellType = ""
        , errMessage = ""
        }
  let resultEx :: SomeException = toException resultError
  assertBool "ResultError is SomePostgreSqlException" $ isJust (fromException resultEx :: Maybe SomePostgreSqlException)
  assertBool "ResultError is ResultError" $ isJust (fromException resultEx :: Maybe ResultError)
  eResultEx :: Either ResultError () <- try $ throwIO resultEx
  assertBool "Can catch ResultError" $ isLeft eResultEx
  eResultPostgreSqlEx :: Either SomePostgreSqlException () <- try $ throwIO resultEx
  assertBool "Can catch SomePostgreSqlException from ResultError" $ isLeft eResultPostgreSqlEx
