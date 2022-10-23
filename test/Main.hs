{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
#endif
module Main (main) where

import Common
import Database.PostgreSQL.Simple.Copy
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.HStore
import Database.PostgreSQL.Simple.Newtypes
import Database.PostgreSQL.Simple.Internal (breakOnSingleQuestionMark, connectionMayHaveOrphanedStatement)
import Database.PostgreSQL.Simple.Types(Query(..),Values(..), PGArray(..))
import qualified Database.PostgreSQL.Simple.Transaction as ST

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, wait)
import Control.Exception as E
import Control.Monad
import Data.Char
import Data.Foldable (toList)
import Data.List (concat, sort, isInfixOf)
import Data.IORef
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Typeable
import GHC.Generics (Generic)

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text(Text)
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import System.FilePath
import System.Timeout(timeout)
import Data.Time.Compat (getCurrentTime, diffUTCTime)
import System.Environment (getEnvironment)
import qualified System.IO as IO

import Test.Tasty
import Test.Tasty.Golden
import Notify
import Serializable
import Time
import Interval

tests :: TestEnv -> TestTree
tests env = testGroup "tests"
    $ map ($ env)
    [ testBytea
    , testCase "ExecuteMany"          . testExecuteMany
    , testCase "Fold"                 . testFold
    , testCase "Notify"               . testNotify
    , testCase "Serializable"         . testSerializable
    , testCase "Time"                 . testTime
    , testCase "Interval"             . testInterval
    , testCase "Array"                . testArray
    , testCase "Array of nullables"   . testNullableArray
    , testCase "HStore"               . testHStore
    , testCase "citext"               . testCIText
    , testCase "JSON"                 . testJSON
    , testCase "Aeson newtype"        . testAeson
    , testCase "DerivingVia"          . testDerivingVia
    , testCase "Question mark escape" . testQM
    , testCase "Savepoint"            . testSavepoint
    , testCase "Unicode"              . testUnicode
    , testCase "Values"               . testValues
    , testCase "Copy"                 . testCopy
    , testCopyFailures
    , testCase "Double"               . testDouble
    , testCase "1-ary generic"        . testGeneric1
    , testCase "2-ary generic"        . testGeneric2
    , testCase "3-ary generic"        . testGeneric3
    , testCase "Timeout"              . testTimeout
    , testCase "Expected user exceptions"     . testExpectedExceptions
    , testCase "Orphaned running query state mgmt" . testOrphanedRunningQueryStateMgmt
    , testCase "Async exceptions"     . testAsyncExceptionFailure
    , testCase "Query canceled"     . testCanceledQueryExceptions
    , testCase "Connection terminated"     . testConnectionTerminated
    ]

testBytea :: TestEnv -> TestTree
testBytea TestEnv{..} = testGroup "Bytea"
    [ testStr "empty"                  []
    , testStr "\"hello\""              $ map (fromIntegral . fromEnum) ("hello" :: String)
    , testStr "ascending"              [0..255]
    , testStr "descending"             [255,254..0]
    , testStr "ascending, doubled up"  $ doubleUp [0..255]
    , testStr "descending, doubled up" $ doubleUp [255,254..0]
    ]
  where
    testStr label bytes = testCase label $ do
        let bs = B.pack bytes

        [Only h] <- query conn "SELECT md5(?::bytea)" [Binary bs]
        assertBool "Haskell -> SQL conversion altered the string" $ md5 bs == h

        [Only (Binary r)] <- query conn "SELECT ?::bytea" [Binary bs]
        assertBool "SQL -> Haskell conversion altered the string" $ bs == r

    doubleUp = concatMap (\x -> [x, x])

testExecuteMany :: TestEnv -> Assertion
testExecuteMany TestEnv{..} = do
    execute_ conn "CREATE TEMPORARY TABLE tmp_executeMany (i INT, t TEXT, b BYTEA)"

    let rows :: [(Int, String, Binary ByteString)]
        rows = [ (1, "hello", Binary "bye")
               , (2, "world", Binary "\0\r\t\n")
               , (3, "?",     Binary "")
               ]

    count <- executeMany conn "INSERT INTO tmp_executeMany VALUES (?, ?, ?)" rows
    count @?= fromIntegral (length rows)

    rows' <- query_ conn "SELECT * FROM tmp_executeMany"
    rows' @?= rows

    return ()

testFold :: TestEnv -> Assertion
testFold TestEnv{..} = do
    xs <- fold_ conn "SELECT generate_series(1,10000)"
            [] $ \xs (Only x) -> return (x:xs)
    reverse xs @?= ([1..10000] :: [Int])

    ref <- newIORef []
    forEach conn "SELECT * FROM generate_series(1,?) a, generate_series(1,?) b"
      (100 :: Int, 50 :: Int) $ \(a :: Int, b :: Int) -> do
        xs <- readIORef ref
        writeIORef ref $! (a,b):xs
    xs <- readIORef ref
    reverse xs @?= [(a,b) | a <- [1..100], b <- [1..50]]

    -- Make sure fold propagates our exception.
    ref <- newIORef []
    True <- expectError (== TestException) $
              forEach_ conn "SELECT generate_series(1,10)" $ \(Only a) ->
                if a == 5 then do
                  -- Cause a SQL error to trip up CLOSE.
                  True <- expectError isSyntaxError $
                          execute_ conn "asdf"
                  True <- expectError ST.isFailedTransactionError $
                          (query_ conn "SELECT 1" :: IO [(Only Int)])
                  throwIO TestException
                else do
                  xs <- readIORef ref
                  writeIORef ref $! (a :: Int) : xs
    xs <- readIORef ref
    reverse xs @?= [1..4]

    withTransaction conn $ replicateM_ 2 $ do
        xs <- fold_ conn "VALUES (1), (2), (3), (4), (5)"
                [] $ \xs (Only x) -> return (x:xs)
        reverse xs @?= ([1..5] :: [Int])

    ref <- newIORef []
    forEach_ conn "SELECT generate_series(1,101)" $ \(Only a) ->
      forEach_ conn "SELECT generate_series(1,55)" $ \(Only b) -> do
        xs <- readIORef ref
        writeIORef ref $! (a :: Int, b :: Int) : xs
    xs <- readIORef ref
    reverse xs @?= [(a,b) | a <- [1..101], b <- [1..55]]

    xs <- fold_ conn "SELECT 1 WHERE FALSE"
            [] $ \xs (Only x) -> return (x:xs)
    xs @?= ([] :: [Int])

    -- TODO: add more complete tests, e.g.:
    --
    --  * Fold in a transaction
    --
    --  * Fold in a transaction after a previous fold has been performed
    --
    --  * Nested fold

    return ()

queryFailure :: forall a. (FromField a, Typeable a, Show a)
             => Connection -> Query -> a -> Assertion
queryFailure conn q resultType = do
  x :: Either SomeException [Only a] <- E.try $ query_ conn q
  case x of
    Left  _   -> return ()
    Right val -> assertFailure ("Did not fail as expected:  "
                              ++ show q
                              ++ " :: "
                              ++ show (typeOf resultType)
                              ++ " -> " ++ show val)

testArray :: TestEnv -> Assertion
testArray TestEnv{..} = do
    xs <- query_ conn "SELECT '{1,2,3,4}'::_int4"
    xs @?= [Only (V.fromList [1,2,3,4 :: Int])]
    xs <- query_ conn "SELECT '{{1,2},{3,4}}'::_int4"
    xs @?= [Only (V.fromList [V.fromList [1,2],
                              V.fromList [3,4 :: Int]])]
    queryFailure conn "SELECT '{1,2,3,4}'::_int4" (undefined :: V.Vector Bool)
    queryFailure conn "SELECT '{{1,2},{3,4}}'::_int4" (undefined :: V.Vector Int)

testNullableArray :: TestEnv -> Assertion
testNullableArray TestEnv{..} = do
    xs <- query_ conn "SELECT '{sometext, \"NULL\"}'::_text"
    xs @?= [Only (V.fromList ["sometext", "NULL" :: Text])]
    xs <- query_ conn "SELECT '{sometext, NULL}'::_text"
    xs @?= [Only (V.fromList [Just "sometext", Nothing :: Maybe Text])]
    queryFailure conn "SELECT '{sometext, NULL}'::_text" (undefined :: V.Vector Text)

testHStore :: TestEnv -> Assertion
testHStore TestEnv{..} = do
    execute_ conn "CREATE EXTENSION IF NOT EXISTS hstore"
    roundTrip []
    roundTrip [("foo","bar"),("bar","baz"),("baz","hello")]
    roundTrip [("fo\"o","bar"),("b\\ar","baz"),("baz","\"value\\with\"escapes")]
  where
    roundTrip :: [(Text,Text)] -> Assertion
    roundTrip xs = do
      let m = Only (HStoreMap (Map.fromList xs))
      m' <- query conn "SELECT ?::hstore" m
      [m] @?= m'

testCIText :: TestEnv -> Assertion
testCIText TestEnv{..} = do
    execute_ conn "CREATE EXTENSION IF NOT EXISTS citext"
    roundTrip (CI.mk "")
    roundTrip (CI.mk "UPPERCASE")
    roundTrip (CI.mk "lowercase")
  where
    roundTrip :: (CI Text) -> Assertion
    roundTrip cit = do
        let toPostgres = Only cit
        fromPostgres <- query conn "SELECT ?::citext" toPostgres
        [toPostgres] @?= fromPostgres

testJSON :: TestEnv -> Assertion
testJSON TestEnv{..} = do
    roundTrip (Map.fromList [] :: Map Text Text)
    roundTrip (Map.fromList [("foo","bar"),("bar","baz"),("baz","hello")] :: Map Text Text)
    roundTrip (Map.fromList [("fo\"o","bar"),("b\\ar","baz"),("baz","\"value\\with\"escapes")] :: Map Text Text)
    roundTrip (V.fromList [1,2,3,4,5::Int])
    roundTrip ("foo" :: Text)
    roundTrip (42 :: Int)
  where
    roundTrip :: ToJSON a => a -> Assertion
    roundTrip a = do
      let js = Only (toJSON a)
      js' <- query conn "SELECT ?::json" js
      [js] @?= js'

testAeson :: TestEnv -> Assertion
testAeson TestEnv{..} = do
    roundTrip (Map.fromList [] :: Map Text Text)
    roundTrip (Map.fromList [("foo","bar"),("bar","baz"),("baz","hello")] :: Map Text Text)
    roundTrip (Map.fromList [("fo\"o","bar"),("b\\ar","baz"),("baz","\"value\\with\"escapes")] :: Map Text Text)
    roundTrip (V.fromList [1,2,3,4,5::Int])
    roundTrip ("foo" :: Text)
    roundTrip (42 :: Int)
  where
    roundTrip :: (Eq a, Show a, Typeable a, ToJSON a, FromJSON a)=> a -> Assertion
    roundTrip x = do
      y <- query conn "SELECT ?::json" (Only (Aeson x))
      [Only (Aeson x)] @?= y

testDerivingVia :: TestEnv -> Assertion
testDerivingVia TestEnv{..} = do
#if __GLASGOW_HASKELL__ <806
    return ()
#else
    roundTrip $ DerivingVia1 42 "Meaning of Life"
  where
    roundTrip :: (Eq a, Show a, Typeable a, ToField a, FromField a)=> a -> Assertion
    roundTrip x = do
      y <- query conn "SELECT ?::json" (Only x)
      [Only x] @?= y

data DerivingVia1 = DerivingVia1 Int String
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving (ToField, FromField) via Aeson DerivingVia1

#endif


testQM :: TestEnv -> Assertion
testQM TestEnv{..} = do
    -- Just test on a single string
    let testQuery' b = "testing for ?" <> b <> " and making sure "
        testQueryDoubleQM = testQuery' "?"
        testQueryRest = "? is substituted"
        testQuery = fromString $ testQueryDoubleQM <> testQueryRest
        -- expect the entire first part with double QMs replaced with literal '?'
        expected = (fromString $ testQuery' "", fromString testQueryRest)
        tried = breakOnSingleQuestionMark testQuery
        errMsg = concat
            [ "Failed to break on single question mark exclusively:\n"
            , "expected: ", show expected
            , "result:   ", show tried
            ]
    assertBool errMsg $ tried == expected

    -- Let's also test the question mark operators in action
    -- ? -> Does the string exist as a top-level key within the JSON value?
    positiveQuery "SELECT ?::jsonb ?? ?" (testObj, "foo" :: Text)
    negativeQuery "SELECT ?::jsonb ?? ?" (testObj, "baz" :: Text)
    negativeQuery "SELECT ?::jsonb ?? ?" (toJSON numArray, "1" :: Text)
    -- ?| -> Do any of these array strings exist as top-level keys?
    positiveQuery "SELECT ?::jsonb ??| ?" (testObj, PGArray ["nope","bar","6" :: Text])
    negativeQuery "SELECT ?::jsonb ??| ?" (testObj, PGArray ["nope","6" :: Text])
    negativeQuery "SELECT ?::jsonb ??| ?" (toJSON numArray, PGArray ["1","2","6" :: Text])
    -- ?& -> Do all of these array strings exist as top-level keys?
    positiveQuery "SELECT ?::jsonb ??& ?" (testObj, PGArray ["foo","bar","quux" :: Text])
    positiveQuery "SELECT ?::jsonb ??& ?" (testObj, PGArray ["foo","bar" :: Text])
    negativeQuery "SELECT ?::jsonb ??& ?" (testObj, PGArray ["foo","bar","baz" :: Text])
    negativeQuery "SELECT ?::jsonb ??& ?" (toJSON numArray, PGArray ["1","2","3","4","5" :: Text])
    -- Format error for 2 question marks, not 4
    True <- expectError (isFormatError 2) $
        (query conn "SELECT ?::jsonb ?? ?" $ Only testObj :: IO [Only Bool])
    return ()
  where positiveQuery :: ToRow a => Query -> a -> Assertion
        positiveQuery = boolQuery True
        negativeQuery :: ToRow a => Query -> a -> Assertion
        negativeQuery = boolQuery False
        numArray :: [Int]
        numArray = [1,2,3,4,5]
        boolQuery :: ToRow a => Bool -> Query -> a -> Assertion
        boolQuery b t x = do
            a <- query conn t x
            [Only b] @?= a
        testObj = toJSON (Map.fromList [("foo",toJSON (1 :: Int))
                                       ,("bar",String "baz")
                                       ,("quux",toJSON [1 :: Int,2,3,4,5])] :: Map Text Value
                         )

testSavepoint :: TestEnv -> Assertion
testSavepoint TestEnv{..} = do
    True <- expectError ST.isNoActiveTransactionError $
            withSavepoint conn $ return ()

    let getRows :: IO [Int]
        getRows = map fromOnly <$> query_ conn "SELECT a FROM tmp_savepoint ORDER BY a"
    withTransaction conn $ do
        execute_ conn "CREATE TEMPORARY TABLE tmp_savepoint (a INT UNIQUE)"
        execute_ conn "INSERT INTO tmp_savepoint VALUES (1)"
        [1] <- getRows

        withSavepoint conn $ do
            execute_ conn "INSERT INTO tmp_savepoint VALUES (2)"
            [1,2] <- getRows
            return ()
        [1,2] <- getRows

        withSavepoint conn $ do
            execute_ conn "INSERT INTO tmp_savepoint VALUES (3)"
            [1,2,3] <- getRows
            True <- expectError isUniqueViolation $
                execute_ conn "INSERT INTO tmp_savepoint VALUES (2)"
            True <- expectError ST.isFailedTransactionError getRows

            -- Body returning successfully after handling error,
            -- but 'withSavepoint' will roll back without complaining.
            return ()
        -- Rolling back clears the error condition.
        [1,2] <- getRows

        -- 'withSavepoint' will roll back after an exception, even if the
        -- exception wasn't SQL-related.
        True <- expectError (== TestException) $
          withSavepoint conn $ do
            execute_ conn "INSERT INTO tmp_savepoint VALUES (3)"
            [1,2,3] <- getRows
            throwIO TestException
        [1,2] <- getRows

        -- Nested savepoint can be rolled back while the
        -- outer effects are retained.
        withSavepoint conn $ do
            execute_ conn "INSERT INTO tmp_savepoint VALUES (3)"
            True <- expectError isUniqueViolation $
              withSavepoint conn $ do
                execute_ conn "INSERT INTO tmp_savepoint VALUES (4)"
                [1,2,3,4] <- getRows
                execute_ conn "INSERT INTO tmp_savepoint VALUES (4)"
            [1,2,3] <- getRows
            return ()
        [1,2,3] <- getRows

        return ()

    -- Transaction committed successfully, even though there were errors
    -- (but we rolled them back).
    [1,2,3] <- getRows

    return ()

testUnicode :: TestEnv -> Assertion
testUnicode TestEnv{..} = do
    let q = Query . T.encodeUtf8  -- Handle encoding ourselves to ensure
                                  -- the table gets created correctly.
    let messages = map Only ["привет","мир"] :: [Only Text]
    execute_ conn (q "CREATE TEMPORARY TABLE ру́сский (сообщение TEXT)")
    executeMany conn "INSERT INTO ру́сский (сообщение) VALUES (?)" messages
    messages' <- query_ conn "SELECT сообщение FROM ру́сский"
    sort messages @?= sort messages'

testValues :: TestEnv -> Assertion
testValues TestEnv{..} = do
    execute_ conn "CREATE TEMPORARY TABLE values_test (x int, y text)"
    test (Values ["int4","text"] [])
    test (Values ["int4","text"] [(1,"hello")])
    test (Values ["int4","text"] [(1,"hello"),(2,"world")])
    test (Values ["int4","text"] [(1,"hello"),(2,"world"),(3,"goodbye")])
    test (Values [] [(1,"hello")])
    test (Values [] [(1,"hello"),(2,"world")])
    test (Values [] [(1,"hello"),(2,"world"),(3,"goodbye")])
  where
    test :: Values (Int, Text) -> Assertion
    test table@(Values _ vals) = do
      execute conn "INSERT INTO values_test ?" (Only table)
      vals' <- query_  conn "DELETE FROM values_test RETURNING *"
      sort vals @?= sort vals'


testCopy :: TestEnv -> Assertion
testCopy TestEnv{..} = do
    execute_ conn "CREATE TEMPORARY TABLE copy_test (x int, y text)"
    copy_ conn "COPY copy_test FROM STDIN (FORMAT CSV)"
    mapM_ (putCopyData conn) copyRows
    putCopyEnd conn
    copy_ conn "COPY copy_test FROM STDIN (FORMAT CSV)"
    mapM_ (putCopyData conn) abortRows
    putCopyError conn "aborted"
    -- Hmm, does postgres always produce \n as an end-of-line here, or
    -- are there cases where it will use a \r\n as well?
    copy_ conn "COPY copy_test TO STDOUT (FORMAT CSV)"
    rows <- loop []
    sort rows @?= sort copyRows
    -- Now, let's just verify that the connection state is back to ready,
    -- so that we can issue more queries:
    [Only (x::Int)] <- query_ conn "SELECT 2 + 2"
    x @?= 4
    -- foldCopyData
    copy_ conn "COPY copy_test TO STDOUT (FORMAT CSV)"
    (acc, count) <- foldCopyData conn
        (\acc row -> return (row:acc))
        (\acc count -> return (acc, count))
        []
    sort acc @?= sort copyRows
    count @?= 2
  where
    copyRows  = ["1,foo\n"
                ,"2,bar\n"]
    abortRows = ["3,baz\n"]
    loop rows = do
      mrow <- getCopyData conn
      case mrow of
        CopyOutDone _   -> return rows
        CopyOutRow  row -> loop (row:rows)

testCopyFailures :: TestEnv -> TestTree
testCopyFailures env = testGroup "Copy failures"
    $ map ($ env)
    [ testCopyUniqueConstraintError
    , testCopyMalformedError
    ]

goldenTest :: TestName -> IO BL.ByteString -> TestTree
goldenTest testName =
    goldenVsString testName (resultsDir </> fileName<.>"expected")
  where
    resultsDir = "test" </> "results"
    fileName = map normalize testName
    normalize c | not (isAlpha c) = '-'
                | otherwise       = c

-- | Test that we provide a sensible error message on failure
testCopyUniqueConstraintError :: TestEnv -> TestTree
testCopyUniqueConstraintError TestEnv{..} =
    goldenTest "unique constraint violation"
    $ handle (\(SomeException exc) -> return $ BL.pack $ show exc) $ do
        execute_ conn "CREATE TEMPORARY TABLE copy_unique_constraint_error_test (x int PRIMARY KEY, y text)"
        copy_ conn "COPY copy_unique_constraint_error_test FROM STDIN (FORMAT CSV)"
        mapM_ (putCopyData conn) copyRows
        _n <- putCopyEnd conn
        return BL.empty
  where
    copyRows  = ["1,foo\n"
                ,"2,bar\n"
                ,"1,baz\n"]

testCopyMalformedError :: TestEnv -> TestTree
testCopyMalformedError TestEnv{..} =
    goldenTest "malformed input"
    $ handle (\(SomeException exc) -> return $ BL.pack $ show exc) $ do
        execute_ conn "CREATE TEMPORARY TABLE copy_malformed_input_error_test (x int PRIMARY KEY, y text)"
        copy_ conn "COPY copy_unique_constraint_error_test FROM STDIN (FORMAT CSV)"
        mapM_ (putCopyData conn) copyRows
        _n <- putCopyEnd conn
        return BL.empty
  where
    copyRows  = ["1,foo\n"
                ,"2,bar\n"
                ,"z,baz\n"]

testTimeout :: TestEnv -> Assertion
testTimeout TestEnv{..} =
    withConn $ \c -> do
      start_t <- getCurrentTime
      res <- timeout 200000 $ do
               withTransaction c $ do
                 query_ c "SELECT pg_sleep(1)" :: IO [Only ()]
      end_t <- getCurrentTime
      assertBool "Timeout did not occur" (res == Nothing)
#if !defined(mingw32_HOST_OS)
-- At the moment, you cannot timely abandon queries with async exceptions on
-- Windows.
      let d = end_t `diffUTCTime` start_t
      assertBool "Timeout didn't work in a timely fashion" (0.1 < d && d < 0.6)
#endif

testDouble :: TestEnv -> Assertion
testDouble TestEnv{..} = do
    [Only (x :: Double)] <- query_ conn "SELECT 'NaN'::float8"
    assertBool "expected NaN" (isNaN x)
    [Only (x :: Double)] <- query_ conn "SELECT 'Infinity'::float8"
    x @?= (1 / 0)
    [Only (x :: Double)] <- query_ conn "SELECT '-Infinity'::float8"
    x @?= (-1 / 0)

-- | Specifies exceptions thrown by postgresql-simple for certain user errors.
testExpectedExceptions :: TestEnv -> Assertion
testExpectedExceptions TestEnv{..} = do
  withConn $ \c -> do
    execute_ c "SELECT 1,2" `shouldThrow` (\(e :: QueryError) -> "2-column result" `isInfixOf` show e)
    execute_ c "SELECT 1/0" `shouldThrow` (\(e :: SqlError) -> sqlState e == "22012")
    (query_ c "SELECT 1, 2, 3" :: IO [(String, Int)]) `shouldThrow` (\(e :: ResultError) -> errSQLType e == "int4" && errHaskellType e == "Text")

shouldThrow :: forall e a. Exception e => IO a -> (e -> Bool) -> IO ()
shouldThrow f pred = do
  ea <- try f
  assertBool "Exception is as expected" $ case ea of
    Right _ -> False
    Left (ex :: e) -> pred ex

-- | Ensures that the state associated with there being an orphaned
-- running statement in a connection is updated accordingly.
testOrphanedRunningQueryStateMgmt :: TestEnv -> Assertion
testOrphanedRunningQueryStateMgmt TestEnv{..} = withConn $ \c -> do
  -- 1. Connections are created with no orphaned running queries, naturally.
  runState c `shouldReturn` False

  -- 2. Interrupting a query that is still running should set the state
  -- to True.
  -- We need to give it enough time to start executing the query
  -- before timing out. One second should be more than enough
  void $ timeout (1000 * 1000) (execute_ c "SELECT pg_sleep(100)")
  runState c `shouldReturn` True

  -- 3. Running a new query should clear the state again
  [ Only (num13 :: Int) ] <- query c "SELECT 13" ()
  num13 @?= 13
  runState c `shouldReturn` False

  -- 4. Interrupting a query but letting it run until completion shouldn't
  -- matter (postgresql-simple has no way of knowing that), but no errors
  -- should come out of it
  void $ timeout (1000 * 1000) (execute_ c "SELECT pg_sleep(2)")
  runState c `shouldReturn` True

  -- One second has passed, wait 2 more to ensure the query finished.
  -- The state is still True.
  threadDelay (1000 * 1000 * 2)
  runState c `shouldReturn` True

  -- 5. Check that nothing wrong happens if we try to cancel a query
  -- that is no longer running (this happens automatically by running another query)
  [ Only (num17 :: Int) ] <- query c "SELECT 17" ()
  num17 @?= 17
  runState c `shouldReturn` False

  -- 6. Other errors that are not interruptions don't change the connection's state
  execute_ c "SELECT 1/0" `shouldThrow` (\(_ :: SqlError) -> True)
  runState c `shouldReturn` False

  where
    runState = readIORef . connectionMayHaveOrphanedStatement
    shouldReturn :: (Eq a, Show a, HasCallStack) => IO a -> a -> IO ()
    shouldReturn f expected = do
        actual <- f
        actual @?= expected


-- | Ensures that asynchronous exceptions thrown while queries are executing
-- are handled properly.
testAsyncExceptionFailure :: TestEnv -> Assertion
testAsyncExceptionFailure TestEnv{..} = withConn $ \c -> do
  -- We need to give it enough time to start executing the query
  -- before timing out. One second should be more than enough
  execute_ c "SET my.setting TO '42'"
  testAsyncException c (1000 * 1000) (execute_ c "SELECT pg_sleep(5)")
  testAsyncException c (1000 * 1000) $
    bracket_ (execute_ c "CREATE TABLE IF NOT EXISTS copy_cancel (v INT)") (execute_ c "DROP TABLE IF EXISTS copy_cancel") $
        bracket_ (copy_ c "COPY copy_cancel FROM STDIN (FORMAT CSV)") (putCopyEnd c) $ do
          putCopyData c "1\n"
          threadDelay (1000 * 1000 * 60)

  where
    testAsyncException c timeLimit f = do
      tmt <- timeout timeLimit f
      tmt @?= Nothing
      -- Any other query should work now without errors.
      number42 <- query_ c "SELECT current_setting('my.setting')"
      number42 @?= [ Only ("42" :: String) ]

-- | Ensures that canceled queries don't invalidate the Connection and specifies how
-- they can be detected.
testCanceledQueryExceptions :: TestEnv -> Assertion
testCanceledQueryExceptions TestEnv{..} = do
  withConn $ \c1 -> withConn $ \c2 -> do
    [ Only (c1Pid :: Int) ] <- query_ c1 "SELECT pg_backend_pid()"
    execute_ c1 "SET my.setting TO '42'"
    
    testCancelation c1 c2 c1Pid execPgSleep $ \(ex :: SqlError) -> sqlState ex == "57014"

    -- What should we expect when COPY is canceled and putCopyEnd runs? The same SqlError as above, perhaps? Right now,
    -- detecting if a query was canceled involves detecting two distinct types of exception.
    testCancelation c1 c2 c1Pid execCopy $ \(ex :: IOException) -> "Database.PostgreSQL.Simple.Copy.putCopyEnd: failed to parse command status" `isInfixOf` show ex
                                                      && "ERROR:  canceling statement due to user request" `isInfixOf` show ex
    
    -- Any other query should work now without errors.
    number42 <- query_ c1 "SELECT current_setting('my.setting')"
    number42 @?= [ Only ("42" :: String) ]

  where
    execPgSleep c = execute_ c "SELECT pg_sleep(5)"
    execCopy c = 
      bracket_ (execute_ c "CREATE TABLE IF NOT EXISTS copy_cancel (v INT)") (execute_ c "DROP TABLE IF EXISTS copy_cancel") $
        bracket_ (copy_ c "COPY copy_cancel FROM STDIN (FORMAT CSV)") (putCopyEnd c) $ do
          putCopyData c "1\n"
          threadDelay (1000 * 1000 * 2)
          -- putCopyEnd will run after pg_cancel_backend due to threadDelays
    testCancelation c1 c2 cPid f exPred = withAsync (f c1) $ \longRunningAction -> do
      -- We need to give it enough time to start executing the query
      -- before canceling it. One second should be more than enough
      threadDelay (1000 * 1000)
      cancelResult <- query c2 "SELECT pg_cancel_backend(?)" (Only cPid)
      cancelResult @?= [ Only True ]
      wait longRunningAction `shouldThrow` exPred
      -- Connection is still usable after query canceled
      [ Only (cPidAgain :: Int) ] <- query_ c1 "SELECT pg_backend_pid()"
      cPid @?= cPidAgain

-- | Ensures that a specific type of exception is thrown when
-- the connection is terminated abruptly.
testConnectionTerminated :: TestEnv -> Assertion
testConnectionTerminated TestEnv{..} = do
  withConn $ \c1 -> withConn $ \c2 -> do
    [ Only (c1Pid :: Int) ] <- query_ c1 "SELECT pg_backend_pid()"
    withAsync (execute_ c1 "SELECT pg_sleep(5)") $ \pgSleep -> do
      -- We need to give it enough time to start executing the query
      -- before terminating it. One second should be more than enough
      threadDelay (1000 * 1000)
      cancelResult <- query c2 "SELECT pg_terminate_backend(?)" (Only c1Pid)
      cancelResult @?= [ Only True ]
      killedQuery <- try $ wait pgSleep
      assertBool "Connection was terminated" $ case killedQuery of
        Right _ -> False
        Left (ex :: SqlError) -> ("server closed the connection unexpectedly" `isInfixOf` show (sqlErrorMsg ex))
                                && sqlExecStatus ex == FatalError

testGeneric1 :: TestEnv -> Assertion
testGeneric1 TestEnv{..} = do
    roundTrip conn (Gen1 123)
  where
    roundTrip conn x0 = do
        r <- query conn "SELECT ?::int" (x0 :: Gen1)
        r @?= [x0]

testGeneric2 :: TestEnv -> Assertion
testGeneric2 TestEnv{..} = do
    roundTrip conn (Gen2 123 "asdf")
  where
    roundTrip conn x0 = do
        r <- query conn "SELECT ?::int, ?::text" x0
        r @?= [x0]

testGeneric3 :: TestEnv -> Assertion
testGeneric3 TestEnv{..} = do
    roundTrip conn (Gen3 123 "asdf" True)
  where
    roundTrip conn x0 = do
        r <- query conn "SELECT ?::int, ?::text, ?::bool" x0
        r @?= [x0]

data Gen1 = Gen1 Int
            deriving (Show,Eq,Generic)
instance FromRow Gen1
instance ToRow   Gen1

data Gen2 = Gen2 Int Text
            deriving (Show,Eq,Generic)
instance FromRow Gen2
instance ToRow   Gen2

data Gen3 = Gen3 Int Text Bool
            deriving (Show,Eq,Generic)
instance FromRow Gen3
instance ToRow   Gen3

data TestException
  = TestException
  deriving (Eq, Show, Typeable)

instance Exception TestException

expectError :: Exception e => (e -> Bool) -> IO a -> IO Bool
expectError p io =
    (io >> return False) `E.catch` \ex ->
    if p ex then return True else throwIO ex

isUniqueViolation :: SqlError -> Bool
isUniqueViolation SqlError{..} = sqlState == "23505"

isSyntaxError :: SqlError -> Bool
isSyntaxError SqlError{..} = sqlState == "42601"

isFormatError :: Int -> FormatError -> Bool
isFormatError i FormatError{..}
    | null fmtMessage = False
    | otherwise = fmtMessage == concat [ show i
                                       , " single '?' characters, but "
                                       , show (length fmtParams)
                                       , " parameters"
                                       ]
------------------------------------------------------------------------

-- | Action for connecting to the database that will be used for testing.
--
-- Note that some tests, such as Notify, use multiple connections, and assume
-- that 'testConnect' connects to the same database every time it is called.
withTestEnv :: ByteString -> (TestEnv -> IO a) -> IO a
withTestEnv connstr cb =
    withConn $ \conn -> do
        -- currently required for interval to work.
        -- we also test that this doesn't interfere with anything else
        execute_ conn "SET intervalstyle TO 'iso_8601'"

        cb TestEnv
            { conn     = conn
            , withConn = withConn
            }
  where
    withConn = bracket (connectPostgreSQL connstr) close

main :: IO ()
main = withConnstring $ \connstring -> do
    IO.hSetBuffering IO.stdout IO.NoBuffering
    IO.hSetBuffering IO.stderr IO.NoBuffering
    withTestEnv connstring (defaultMain . tests)

withConnstring :: (BS8.ByteString -> IO ()) -> IO ()
withConnstring kont = do
    env <- getEnvironment
    case lookup "DATABASE_CONNSTRING" env of
        Just s  -> kont (BS8.pack (special s))
        Nothing -> case lookup "GITHUB_ACTIONS" env of
            Just "true" -> kont (BS8.pack gha)
            _           -> putStrLn "Set DATABASE_CONNSTRING environment variable"
   where
    -- https://www.appveyor.com/docs/services-databases/
    special "appveyor" = "dbname='TestDb' user='postgres' password='Password12!'"
    special "travis"   = ""
    special s          = s

    gha = unwords
        [ "dbname='postgres'"
        , "user='postgres'"
        , "password='postgres'"
        , "host='postgres'"
        , "port=5432"
        ]
