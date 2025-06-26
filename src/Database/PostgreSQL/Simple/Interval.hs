{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE NumDecimals #-}

module Database.PostgreSQL.Simple.Interval where

import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.Function as Function
import qualified Data.Int as Int
import qualified Data.Scientific as Scientific
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField as Postgres
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as Postgres

data Interval = MkInterval
  { months :: !Int.Int32,
    days :: !Int.Int32,
    microseconds :: !Int.Int64
  }
  deriving (Eq, Show)

instance Postgres.FromField Interval where
  fromField = Postgres.attoFieldParser (== Postgres.intervalOid) parse

instance Postgres.ToField Interval where
  toField = Postgres.Plain . render

zero :: Interval
zero = MkInterval 0 0 0

fromMicroseconds :: Int.Int64 -> Interval
fromMicroseconds x = zero {microseconds = x}

fromMilliseconds :: Int.Int64 -> Maybe Interval
fromMilliseconds =
  fmap fromMicroseconds
    . Bits.toIntegralSized
    . (*) 1e3
    . toInteger

fromSeconds :: Int.Int64 -> Maybe Interval
fromSeconds =
  fmap fromMicroseconds
    . Bits.toIntegralSized
    . (*) 1e6
    . toInteger

fromMinutes :: Int.Int64 -> Maybe Interval
fromMinutes =
  fmap fromMicroseconds
    . Bits.toIntegralSized
    . (*) 60e6
    . toInteger

fromHours :: Int.Int64 -> Maybe Interval
fromHours =
  fmap fromMicroseconds
    . Bits.toIntegralSized
    . (*) 3600e6
    . toInteger

fromDays :: Int.Int32 -> Interval
fromDays x = zero {days = x}

fromWeeks :: Int.Int32 -> Maybe Interval
fromWeeks =
  fmap fromDays
    . Bits.toIntegralSized
    . (*) 7
    . toInteger

fromMonths :: Int.Int32 -> Interval
fromMonths x = zero {months = x}

fromYears :: Int.Int32 -> Maybe Interval
fromYears =
  fmap fromMonths
    . Bits.toIntegralSized
    . (*) 12
    . toInteger

add :: Interval -> Interval -> Maybe Interval
add x y =
  let safeAdd :: (Bits.Bits a, Integral a) => a -> a -> Maybe a
      safeAdd n = Bits.toIntegralSized . Function.on (+) toInteger n
   in MkInterval
        <$> Function.on safeAdd months x y
        <*> Function.on safeAdd days x y
        <*> Function.on safeAdd microseconds x y

render :: Interval -> Builder.Builder
render x =
  let signed :: (Num a, Ord a) => (a -> Builder.Builder) -> a -> Builder.Builder
      signed f n = (if n > 0 then "+" else "") <> f n
   in "interval '"
        <> signed Builder.int32Dec (months x)
        <> " months "
        <> signed Builder.int32Dec (days x)
        <> " days "
        <> signed Builder.int64Dec (microseconds x)
        <> " microseconds'"

parse :: A.Parser Interval
parse =
  -- Start with parsers that have non-empty prefixes, in order to avoid
  -- ambiguity. Neither of the `postgres` nor `sql_standard` interval styles
  -- have a prefix (or suffix), so whichever one is attempted first needs to
  -- make sure it has consumed all of the input.
  A.choice $
    parseInfinities
      : fmap
        (fromComponents =<<)
        [ parseIso8601,
          parsePostgresVerbose,
          parsePostgres <* A.endOfInput,
          parseSqlStandard
        ]

parseInfinities :: A.Parser Interval
parseInfinities =
  -- Both `-infinity` and `infinity` are new as of PostgreSQL 17.0.
  -- https://www.postgresql.org/message-id/E1r2rB1-005PHm-UL%40gemulon.postgresql.org
  A.choice
    [ MkInterval minBound minBound minBound <$ "-infinity",
      MkInterval maxBound maxBound maxBound <$ "infinity"
    ]

parseIso8601 :: A.Parser [Component]
parseIso8601 = do
  Monad.void "P"
  dates <-
    A.many' $
      A.choice
        [ Years <$> A.signed A.decimal <* "Y",
          Months <$> A.signed A.decimal <* "M",
          Days <$> A.signed A.decimal <* "D"
        ]
  times <- A.option [] $ do
    Monad.void "T"
    A.many' $
      A.choice
        [ Hours <$> A.signed A.decimal <* "H",
          Minutes <$> A.signed A.decimal <* "M",
          Microseconds <$> A.signed A.scientific <* "S"
        ]
  pure $ dates <> times

parsePostgresVerbose :: A.Parser [Component]
parsePostgresVerbose = do
  Monad.void "@ "
  components <-
    flip A.sepBy " " $
      A.choice
        [ Years <$> A.signed A.decimal <* maybePlural " year",
          Months <$> A.signed A.decimal <* maybePlural " mon",
          Days <$> A.signed A.decimal <* maybePlural " day",
          Hours <$> A.signed A.decimal <* maybePlural " hour",
          Minutes <$> A.signed A.decimal <* maybePlural " min",
          Microseconds <$> A.signed A.scientific <* A.option "" (maybePlural " sec")
        ]
  ago <- A.option "" " ago"
  pure $ negateComponentsWhen (not $ ByteString.null ago) components

parsePostgres :: A.Parser [Component]
parsePostgres = do
  dates <-
    flip A.sepBy " " $
      A.choice
        [ Years <$> A.signed A.decimal <* maybePlural " year",
          Months <$> A.signed A.decimal <* maybePlural " mon",
          Days <$> A.signed A.decimal <* maybePlural " day"
        ]
  time <- A.option [] $ A.skipSpace *> parseTime
  pure $ dates <> time

parseSqlStandard :: A.Parser [Component]
parseSqlStandard = do
  let parseYearsAndMonths = do
        sign <- parseSign
        years <- Years <$> A.decimal <* "-"
        months_ <- Months <$> A.decimal
        pure $ negateComponentsWhen (sign == "-") [years, months_]
  let parseDays = (: []) . Days <$> A.signed A.decimal
  let parsers = [parseYearsAndMonths, parseTime, parseDays]
  mconcat <$> A.sepBy1 (A.choice parsers) " "

parseTime :: A.Parser [Component]
parseTime = do
  sign <- parseSign
  hours <- Hours <$> A.decimal <* ":"
  minutes <- Minutes <$> A.decimal <* ":"
  micros <- Microseconds <$> A.scientific
  pure $ negateComponentsWhen (sign == "-") [hours, minutes, micros]

parseSign :: A.Parser ByteString.ByteString
parseSign = A.choice ["-", "+", ""]

maybePlural :: ByteString.ByteString -> A.Parser ByteString.ByteString
maybePlural word = (<>) <$> A.string word <*> A.option "" "s"

data Component
  = Years !Integer
  | Months !Integer
  | Days !Integer
  | Hours !Integer
  | Minutes !Integer
  | Microseconds !Scientific.Scientific
  deriving (Eq, Show)

fromComponent :: Component -> Maybe Interval
fromComponent c = case c of
  Years y -> fromYears =<< Bits.toIntegralSized y
  Months m -> fromMonths <$> Bits.toIntegralSized m
  Days d -> fromDays <$> Bits.toIntegralSized d
  Hours h -> fromHours =<< Bits.toIntegralSized h
  Minutes m -> fromMinutes =<< Bits.toIntegralSized m
  Microseconds u -> fromMicroseconds <$> Scientific.toBoundedInteger (u * 1e6)

fromComponents ::
  (Applicative.Alternative f, Traversable t) =>
  t Component ->
  f Interval
fromComponents =
  maybe Applicative.empty pure
    . (Monad.foldM add zero Monad.<=< traverse fromComponent)

negateComponent :: Component -> Component
negateComponent c = case c of
  Years y -> Years -y
  Months m -> Months -m
  Days d -> Days -d
  Hours h -> Hours -h
  Minutes m -> Minutes -m
  Microseconds u -> Microseconds -u

negateComponentsWhen :: (Functor f) => Bool -> f Component -> f Component
negateComponentsWhen p = if p then fmap negateComponent else id
