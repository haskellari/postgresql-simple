{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Exception (testExceptions) where

import Database.PostgreSQL.Simple
import Test.Tasty.HUnit (Assertion, assertBool)
import Common (TestEnv)
import Control.Exception (Exception (..), SomeException)
import Data.Maybe ( isJust )

testExceptions :: TestEnv -> Assertion
testExceptions _ = do
  let sqlError :: SomeException 
        = toException $ SqlError 
        { sqlState       = ""
        , sqlExecStatus  = FatalError
        , sqlErrorMsg    = ""
        , sqlErrorDetail = ""
        , sqlErrorHint   = ""
        }
  assertBool "SqlError is SomepostgreSqlException" $ isJust (fromException sqlError :: Maybe SomePostgreSqlException)
  assertBool "SqlError is SqlError" $ isJust (fromException sqlError :: Maybe SqlError)
  
  let formatError :: SomeException 
        = toException $ FormatError 
        { fmtMessage = ""
        , fmtQuery = ""
        , fmtParams = []
        }
  assertBool "FormatError is SomepostgreSqlException" $ isJust (fromException formatError :: Maybe SomePostgreSqlException)
  assertBool "FormatError is FormatError" $ isJust (fromException formatError :: Maybe FormatError)

  let queryError :: SomeException 
        = toException $ QueryError 
        { qeMessage = ""
        , qeQuery = ""
        }
  assertBool "QueryError is SomepostgreSqlException" $ isJust (fromException queryError :: Maybe SomePostgreSqlException)
  assertBool "QueryError is QueryError" $ isJust (fromException queryError :: Maybe QueryError)
  
  let resultError :: SomeException
        = toException $ Incompatible 
        { errSQLType = ""
        , errSQLTableOid = Nothing
        , errSQLField = ""
        , errHaskellType = ""
        , errMessage = ""
        }
  assertBool "ResultError is SomepostgreSqlException" $ isJust (fromException resultError :: Maybe SomePostgreSqlException)
  assertBool "ResultError is ResultError" $ isJust (fromException resultError :: Maybe ResultError)
