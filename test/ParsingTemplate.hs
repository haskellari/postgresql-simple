{-# LANGUAGE OverloadedStrings #-}
module ParsingTemplate (testParsingTemplate) where

import Common
import Test.Tasty.HUnit (assertBool, assertEqual)
import Database.PostgreSQL.Simple (parseTemplate)

testParsingTemplate :: TestEnv -> Assertion
testParsingTemplate env@TestEnv{..} = do
  assertEqual "" (Just ("values ","(?, ?)","")) (parseTemplate "values (?, ?)")
  assertEqual "" (Just ("values ","(?, X(?))","")) (parseTemplate "values (?, X(?))")
