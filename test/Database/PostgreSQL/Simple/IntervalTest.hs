{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE NumDecimals #-}

module Database.PostgreSQL.Simple.IntervalTest where

import Common
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as Ascii
import Data.Functor ((<&>))
import qualified Data.Int as Int
import qualified Database.PostgreSQL.Simple.Interval as I
import Test.Tasty

testTree :: TestEnv -> TestTree
testTree _ =
  testGroup
    "Database.PostgreSQL.Simple.Interval"
    [ testGroup
        "zero"
        [ testCase "works" $ do
            let actual = I.zero
            actual @?= I.MkInterval 0 0 0
        ],
      testGroup
        "add"
        [ testCase "succeeds with no overflow" $ do
            let actual = I.add (I.MkInterval 1 2 3) (I.MkInterval 4 5 6)
            actual @?= Just (I.MkInterval 5 7 9),
          testCase "fails with positive month overflow" $ do
            let actual = I.add (I.fromMonths maxBound) (I.fromMonths 1)
            actual @?= Nothing,
          testCase "fails with negative month overflow" $ do
            let actual = I.add (I.fromMonths minBound) (I.fromMonths -1)
            actual @?= Nothing,
          testCase "fails with positive day overflow" $ do
            let actual = I.add (I.fromDays maxBound) (I.fromDays 1)
            actual @?= Nothing,
          testCase "fails with negative day overflow" $ do
            let actual = I.add (I.fromDays minBound) (I.fromDays -1)
            actual @?= Nothing,
          testCase "fails with positive microsecond overflow" $ do
            let actual = I.add (I.fromMicroseconds maxBound) (I.fromMicroseconds 1)
            actual @?= Nothing,
          testCase "fails with negative microsecond overflow" $ do
            let actual = I.add (I.fromMicroseconds minBound) (I.fromMicroseconds -1)
            actual @?= Nothing
        ],
      testGroup
        "fromMicroseconds"
        [ testCase "works" $ do
            let actual = I.fromMicroseconds 1
            actual @?= I.MkInterval 0 0 1
        ],
      testGroup
        "fromMilliseconds"
        [ testCase "succeeds with no overflow" $ do
            let actual = I.fromMilliseconds 1
            actual @?= Just (I.MkInterval 0 0 1e3),
          testCase "fails with overflow" $ do
            let actual = I.fromMilliseconds maxBound
            actual @?= Nothing
        ],
      testGroup
        "fromSeconds"
        [ testCase "succeeds with no overflow" $ do
            let actual = I.fromSeconds 1
            actual @?= Just (I.MkInterval 0 0 1e6),
          testCase "fails with overflow" $ do
            let actual = I.fromSeconds maxBound
            actual @?= Nothing
        ],
      testGroup
        "fromMinutes"
        [ testCase "succeeds with no overflow" $ do
            let actual = I.fromMinutes 1
            actual @?= Just (I.MkInterval 0 0 60e6),
          testCase "fails with overflow" $ do
            let actual = I.fromMinutes maxBound
            actual @?= Nothing
        ],
      testGroup
        "fromHours"
        [ testCase "succeeds with no overflow" $ do
            let actual = I.fromHours 1
            actual @?= Just (I.MkInterval 0 0 3600e6),
          testCase "fails with overflow" $ do
            let actual = I.fromHours maxBound
            actual @?= Nothing
        ],
      testGroup
        "fromDays"
        [ testCase "works" $ do
            let actual = I.fromDays 1
            actual @?= I.MkInterval 0 1 0
        ],
      testGroup
        "fromWeeks"
        [ testCase "succeeds with no overflow" $ do
            let actual = I.fromWeeks 1
            actual @?= Just (I.MkInterval 0 7 0),
          testCase "fails with overflow" $ do
            let actual = I.fromWeeks maxBound
            actual @?= Nothing
        ],
      testGroup
        "fromMonths"
        [ testCase "works" $ do
            let actual = I.fromMonths 1
            actual @?= I.MkInterval 1 0 0
        ],
      testGroup
        "fromYears"
        [ testCase "succeeds with no overflow" $ do
            let actual = I.fromYears 1
            actual @?= Just (I.MkInterval 12 0 0),
          testCase "fails with overflow" $ do
            let actual = I.fromYears maxBound
            actual @?= Nothing
        ],
      testGroup
        "render"
        [ testCase "works with zero" $ do
            let actual = Builder.toLazyByteString $ I.render I.zero
            actual @?= "interval '0 months 0 days 0 microseconds'",
          testCase "works with positive components" $ do
            let actual = Builder.toLazyByteString . I.render $ I.MkInterval 1 2 3
            actual @?= "interval '+1 months +2 days +3 microseconds'",
          testCase "works with negative components" $ do
            let actual = Builder.toLazyByteString . I.render $ I.MkInterval -3 -2 -1
            actual @?= "interval '-3 months -2 days -1 microseconds'"
        ],
      testGroup
        "parse"
        [ testCase "fails with invalid input" $ do
            let actual = Attoparsec.parseOnly I.parse "invalid"
            actual @?= Left "Failed reading: empty",
          testCase "succeeds with positive infinity" $ do
            let actual = Attoparsec.parseOnly I.parse "infinity"
            actual @?= Right (I.MkInterval maxBound maxBound maxBound),
          testCase "succeeds with negative infinity" $ do
            let actual = Attoparsec.parseOnly I.parse "-infinity"
            actual @?= Right (I.MkInterval minBound minBound minBound),
          testGroup "styles" $
            intervalStyles <&> \(style, field) ->
              testGroup style $
                examples <&> \example ->
                  let input = field example
                   in testCase (Ascii.unpack input) $ do
                        let actual = Attoparsec.parseOnly I.parse input
                        actual @?= Right (exampleInterval example)
        ]
    ]

data Example = MkExample
  { exampleInterval :: I.Interval,
    exampleIso8601 :: Ascii.ByteString,
    examplePostgres :: Ascii.ByteString,
    examplePostgresVerbose :: Ascii.ByteString,
    exampleSqlStandard :: Ascii.ByteString
  }
  deriving (Eq, Show)

intervalStyles :: [(String, Example -> Ascii.ByteString)]
intervalStyles =
  [ ("iso_8601", exampleIso8601),
    ("postgres", examplePostgres),
    ("postgres_verbose", examplePostgresVerbose),
    ("sql_standard", exampleSqlStandard)
  ]

mkExample ::
  Int.Int32 ->
  Int.Int32 ->
  Int.Int64 ->
  Ascii.ByteString ->
  Ascii.ByteString ->
  Ascii.ByteString ->
  Ascii.ByteString ->
  Example
mkExample m d s iso8601 postgres postgresVerbose sqlStandard =
  MkExample
    { exampleInterval = I.MkInterval {I.months = m, I.days = d, I.microseconds = s},
      exampleIso8601 = iso8601,
      examplePostgres = postgres,
      examplePostgresVerbose = postgresVerbose,
      exampleSqlStandard = sqlStandard
    }

examples :: [Example]
examples =
  [ mkExample 0 0 0 "PT0S" "00:00:00" "@ 0" "0",
    mkExample 1 0 0 "P1M" "1 mon" "@ 1 mon" "0-1",
    mkExample -1 0 0 "P-1M" "-1 mons" "@ 1 mon ago" "-0-1",
    mkExample 3 0 0 "P3M" "3 mons" "@ 3 mons" "0-3",
    mkExample 6 0 0 "P6M" "6 mons" "@ 6 mons" "0-6",
    mkExample 12 0 0 "P1Y" "1 year" "@ 1 year" "1-0",
    mkExample -12 0 0 "P-1Y" "-1 years" "@ 1 year ago" "-1-0",
    mkExample 13 0 0 "P1Y1M" "1 year 1 mon" "@ 1 year 1 mon" "1-1",
    mkExample -13 0 0 "P-1Y-1M" "-1 years -1 mons" "@ 1 year 1 mon ago" "-1-1",
    mkExample 24 0 0 "P2Y" "2 years" "@ 2 years" "2-0",
    mkExample 0 1 0 "P1D" "1 day" "@ 1 day" "1 0:00:00",
    mkExample 0 -1 0 "P-1D" "-1 days" "@ 1 day ago" "-1 0:00:00",
    mkExample 0 2 0 "P2D" "2 days" "@ 2 days" "2 0:00:00",
    mkExample 0 7 0 "P7D" "7 days" "@ 7 days" "7 0:00:00",
    mkExample 0 0 1 "PT0.000001S" "00:00:00.000001" "@ 0.000001 secs" "0:00:00.000001",
    mkExample 0 0 -1 "PT-0.000001S" "-00:00:00.000001" "@ 0.000001 secs ago" "-0:00:00.000001",
    mkExample 0 0 1e3 "PT0.001S" "00:00:00.001" "@ 0.001 secs" "0:00:00.001",
    mkExample 0 0 1e6 "PT1S" "00:00:01" "@ 1 sec" "0:00:01",
    mkExample 0 0 -1e6 "PT-1S" "-00:00:01" "@ 1 sec ago" "-0:00:01",
    mkExample 0 0 2e6 "PT2S" "00:00:02" "@ 2 secs" "0:00:02",
    mkExample 0 0 60e6 "PT1M" "00:01:00" "@ 1 min" "0:01:00",
    mkExample 0 0 -60e6 "PT-1M" "-00:01:00" "@ 1 min ago" "-0:01:00",
    mkExample 0 0 120e6 "PT2M" "00:02:00" "@ 2 mins" "0:02:00",
    mkExample 0 0 3600e6 "PT1H" "01:00:00" "@ 1 hour" "01:00:00",
    mkExample 0 0 -3600e6 "PT-1H" "-01:00:00" "@ 1 hour ago" "-01:00:00",
    mkExample 0 0 7200e6 "PT2H" "02:00:00" "@ 2 hours" "02:00:00",
    mkExample 0 0 86400e6 "PT24H" "24:00:00" "@ 24 hours" "24:00:00",
    mkExample 1 1 1e6 "P1M1DT1S" "1 mon 1 day 00:00:01" "@ 1 mon 1 day 1 sec" "+0-1 +1 +0:00:01",
    mkExample -1 -1 -1e6 "P-1M-1DT-1S" "-1 mons -1 days -00:00:01" "@ 1 mon 1 day 1 sec ago" "-0-1 -1 -0:00:01",
    mkExample -1 1 1e6 "P-1M1DT1S" "-1 mons +1 day 00:00:01" "@ 1 mon -1 days -1 sec ago" "-0-1 +1 +0:00:01",
    mkExample 1 -1 1e6 "P1M-1DT1S" "1 mon -1 days +00:00:01" "@ 1 mon -1 days 1 sec" "+0-1 -1 +0:00:01",
    mkExample 1 1 -1e6 "P1M1DT-1S" "1 mon 1 day -00:00:01" "@ 1 mon 1 day -1 sec" "+0-1 +1 -0:00:01",
    mkExample 14 3 14706000007 "P1Y2M3DT4H5M6.000007S" "1 year 2 mons 3 days 04:05:06.000007" "@ 1 year 2 mons 3 days 4 hours 5 mins 6.000007 secs" "+1-2 +3 +4:05:06.000007",
    mkExample maxBound 0 0 "P178956970Y7M" "178956970 years 7 mons" "@ 178956970 years 7 mons" "178956970-7",
    mkExample minBound 0 0 "P-178956970Y-8M" "-178956970 years -8 mons" "@ 178956970 years 8 mons ago" "-178956970-8",
    mkExample 0 maxBound 0 "P2147483647D" "2147483647 days" "@ 2147483647 days" "2147483647 0:00:00",
    mkExample 0 minBound 0 "P-2147483648D" "-2147483648 days" "@ 2147483648 days ago" "-2147483648 0:00:00",
    mkExample 0 0 maxBound "PT2562047788H54.775807S" "2562047788:00:54.775807" "@ 2562047788 hours 54.775807 secs" "2562047788:00:54.775807",
    mkExample 0 0 minBound "PT-2562047788H-54.775808S" "-2562047788:00:54.775808" "@ 2562047788 hours 54.775808 secs ago" "-2562047788:00:54.775808"
  ]
