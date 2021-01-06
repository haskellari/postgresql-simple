{-# LANGUAGE QuasiQuotes #-}

{-

Testing strategies:

fromString . toString == id           ** Todo?

toString . fromString == almost id    ** Todo?

postgresql -> haskell -> postgresql   *  Done

haskell -> postgresql -> haskell      ** Todo?

But still,  what we really want to establish is that the two values
correspond;  for example,  a conversion that consistently added hour
when printed to a string and subtracted an hour when parsed from string
would still pass these tests.


Right now,  we are checking that 1400+ timestamps in the range of 1860 to
2060 round trip from postgresql to haskell and back in 5 different timezones.
In addition to UTC,  the four timezones were selected so that 2 have a positive
offset,  and 2 have a negative offset,   and that 2 have an offset of a
whole number of hours,  while the other two do not.

It may be worth adding a few more timezones to ensure better test coverage.

We are checking a handful of selected timestamps to ensure we hit
various corner-cases in the code,  in addition to 1400 timestamps randomly
generated with granularity of seconds down to microseconds in powers of ten.

-}

module Interval (testInterval) where

import Common
import Control.Monad(forM_, replicateM_)
import Data.Time.Compat
import Data.Time.LocalTime.Compat (CalendarDiffTime(..))
import Data.ByteString(ByteString)
import Database.PostgreSQL.Simple.SqlQQ

data IntervalTestCase = IntervalTestCase
  { label :: String
  , inputMonths :: Integer
  , inputSeconds :: NominalDiffTime
  , asText :: String
  }
  deriving (Eq, Show)

testInterval :: TestEnv -> Assertion
testInterval env@TestEnv{..} = do

  initializeTable env

  let milliseconds = 0.001
      seconds = 1
      minutes = 60 * seconds
      hours = 60 * minutes
      days = 24 * hours
      weeks = 7 * days
      months = 1
      years = 12 * months

  mapM (checkRoundTrip env)
    [ IntervalTestCase
      { label = "zero"
      , inputMonths = 0
      , inputSeconds = 0
      , asText = "PT0"
      }
    , IntervalTestCase
      { label = "1 year"
      , inputMonths = 1 * years
      , inputSeconds = 0
      , asText = "P1Y"
      }
    , IntervalTestCase
      { label = "2 months"
      , inputMonths = 2 * months
      , inputSeconds = 0
      , asText = "P2M"
      }
    , IntervalTestCase
      { label = "3 weeks"
      , inputMonths = 0
      , inputSeconds = 3 * weeks
      , asText = "P3W"
      }
    , IntervalTestCase
      { label = "4 days"
      , inputMonths = 0
      , inputSeconds = 4 * days
      , asText = "P4D"
      }
    , IntervalTestCase
      { label = "5 hours"
      , inputMonths = 0
      , inputSeconds = 5 * hours
      , asText = "PT5H"
      }
    , IntervalTestCase
      { label = "6 minutes"
      , inputMonths = 0
      , inputSeconds = 6 * minutes
      , asText = "PT6M"
      }
    , IntervalTestCase
      { label = "7 seconds"
      , inputMonths = 0
      , inputSeconds = 7 * seconds
      , asText = "PT7S"
      }
    , IntervalTestCase
      { label = "8 milliseconds"
      , inputMonths = 0
      , inputSeconds = 8 * milliseconds
      , asText = "PT0.008S"
      }
    , IntervalTestCase
      { label = "combination of intervals (day-size or bigger)"
      , inputMonths = 2 * years + 4 * months
      , inputSeconds = 3 * weeks + 5 * days
      , asText = "P2Y4M3W5D"
      }
    , IntervalTestCase
      { label = "combination of intervals (smaller than day-size)"
      , inputMonths = 0
      , inputSeconds = 18 * hours + 56 * minutes + 23 * seconds + 563 * milliseconds
      , asText = "PT18H56M23.563S"
      }
    , IntervalTestCase
      { label = "full combination of intervals"
      , inputMonths = 2 * years + 4 * months
      , inputSeconds = 3 * weeks + 5 * days + 18 * hours + 56 * minutes + 23 * seconds + 563 * milliseconds
      , asText = "P2Y4M3W5DT18H56M23.563S"
      }
    ]

  return ()

initializeTable :: TestEnv -> IO ()
initializeTable TestEnv{..} = withTransaction conn $ do
  execute_ conn
     [sql| CREATE TEMPORARY TABLE testinterval
             ( id serial, sample interval, PRIMARY KEY(id) ) |]

  return ()

checkRoundTrip :: TestEnv -> IntervalTestCase -> IO ()
checkRoundTrip TestEnv{..} IntervalTestCase{..} = do

  let input = CalendarDiffTime
        { ctMonths = inputMonths
        , ctTime = inputSeconds
        }

  [(returnedId :: Int, output :: CalendarDiffTime)] <- query conn [sql|
      INSERT INTO testinterval (sample)
      VALUES (?)
      RETURNING id, sample
    |] (Only input)

  assertBool ("CalendarDiffTime did not round-trip from Haskell to SQL and back (" ++ label ++ ")") $
      output == input

  [(Only isExpectedIso)] <- query conn [sql|
      SELECT sample = (?)::interval
      FROM testinterval
      WHERE id = ?
    |] (asText, returnedId)

  assertBool ("CalendarDiffTime inserted did not match ISO8601 equivalent \"" ++ asText ++ "\". (" ++ label ++ ")")
    isExpectedIso

