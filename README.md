# Rel8

Welcome to Rel8! Rel8 is an API built on top of the
fantastic [Opaleye](https://hackage.haskell.org/package/opaleye) library to
provide an easy and type-safe way to interact with relational databases.

## Differences With Opaleye

### Table Definition

Opaleye doesn't really prescribe much in the way of table definition. Perhaps
the most idiomatic approach is to create a record where each field of the record
is parameterized. Then you provide type aliases that either use concrete Haskell
types or the `Column` type. The Opaleye tutorial demonstrates this as

```haskell
data Birthday' a b = Birthday { bdName :: a, bdDay :: b }
type Birthday = Birthday' String Day
type BirthdayColumn = Birthday' (Column PGText) (Column PGDate)
```

In Rel8, the idiomatic approach is to create a record per table, but we use a
single type parameter to denote "where" this data is, and a type family to
interpret each column. The above example would be written as

```haskell
data Birthday f = Birthday { bdName :: Anon f Text , bdDay :: Anon f Day}
instance Table (Birthday Expr) (Birthday QueryResult)
```

### Schema Declaration

In Opaleye, the schema for a table is a value that you need to provide
explicitly. For the `Birthday'` type above, in Opaleye you would write

```haskell
birthdayTable :: Table BirthdayColumn BirthdayColumn
birthdayTable = Table "birthdayTable"
                       (pBirthday Birthday { bdName = required "name"
                                           , bdDay  = required "birthday" })
```

In Rel8, the schema is directly specified in the type. If `birthday` is a table,
then our `Birthday` record would be written as:

```haskell
data Birthday f = Birthday 
  { bdName :: Col f "name" 'NoDefault Text
  , bdName :: Col f "birthday" 'NoDefault Day
  } deriving (Generic)

instance Table (Birthday Expr) (Birthday QueryResult)
instance BaseTable Birthday where tableName = "birthdayTable"
```

### Column Types

In Opaleye, column types form a distinct universe from ordinary Haskell types.
When we define tables, we use types such as `PGText` and `PGDate`.

In Rel8, you work entirely with the "result" types - the result of actually
querying data from the database. Haskell types map to exactly one type in the
database - `Text` is `text`, `Int64` is `bigint`, and so on. This mapping is
captured by the `DBType` type class.

### Aggregation

In Opaleye, aggregation is performed by using the `aggregate` function which
requires an `Aggregator`. Due to
the
[particularities of SQL](https://github.com/tomjaguarpaw/haskell-opaleye/issues/282),
`Aggregators` are not `Arrow`s, nor are they functions. This leaves us with
little option to build `Aggregator`s, though with `ProductProfunctor` (and some
template Haskell), the pain is somewhat eased. From the basic tutorial:

```haskell
aggregateWidgets :: Query (Widget (Column PGText) (Column PGText) (Column PGInt8)
                                  (Column PGInt4) (Column PGFloat8))
aggregateWidgets = aggregate (pWidget (Widget { style    = groupBy
                                              , color    = groupBy
                                              , location = count
                                              , quantity = sum
                                              , radius   = avg }))
                             (queryTable widgetTable)
```

This same approach is compatible with Rel8, but Rel8 has an alternative way to
perform aggregating. In Rel8, we have

```haskell
aggregate :: AggregateTable exprsIn exprsOut => Query exprsIn -> Query exprsOut
```

This means that we can `aggregate` any `Query`, provided the result of that
query is "valid" for aggregation. In Rel8, the above could be written as:

```haskell
aggregateWidgets :: Query (Widget Expr)
aggregateWidgets = aggregate $ proc _ -> do
  widget <- queryTable -< ()
  returnA -< Widget { style = groupBy
                    , color = groupBy
                    , location = count
                    , quantity = sum
                    , radius = avg
                    }

instance AggregateTable (Widget Aggregate) (Widget Expr)
```

While the two seem similar, I have found the latter to be a little easier to
work with, though the former is arguably closer to normal Haskell code
(`aggregate` being similar to `foldMap`). Personally, I'm not entirely happy
with either, so this space may change!

### `Column` vs `Expr`

The `Column` and `Expr` types are fundamentally same, but in order to avoid
orphan instances I currently need to provide a `Expr` `newtype`. While `Column`
should (morally) scope over `PG` types (`PGText`, `PGBool`, etc), `Expr` scopes
over Haskell types.

### Aggregation Functions

The built in aggregation functions in Rel8 are a little bit more honest to how
things work in PostgreSQL, at the expense of being less idiomatic Haskell. As
PostgreSQL has overloaded functions, the aggregations are also overloaded
functions provided by type classes. For example, we have

```haskell
sum :: Expr Int16 -> Aggreagte Int64
sum :: Expr Int64 -> Aggregate Scientific
sum :: Expr Double -> Aggreagte Double
```

### Outer Joins

Rel8 contains a row transforming type `MaybeTable` to capture the result of
outer joins. Opaleye deals with this by the use of `NullMaker`s. `MaybeTable`s,
when selected, will return `Maybe` of the actual row itself. You can project
columns out of a `MaybeTable` with the `$?` operator (function application on a
possibly-`null` row).

### `NULL`

Rel8 accepts the reality that SQL has `null` as a fundamental concept, and
provides operators over both values and `null`. That is, while we have the
ordinary `==.` operator:

```haskell
(==.) :: DBEq a => Expr a -> Expr a -> Expr Bool
```

We have the same operator, but lifted to operate on `null`:

```haskell
(==?) :: DBEq a => Expr (Maybe a) -> Expr (Maybe a) -> Expr (Maybe Bool)
```

While this unfortunately doubles the API, it's necessary - at least if you want
to write performant code. The problem arises in PostgreSQL itself. Consider a
table `t` with column `a` that can contain `null`, and suppose we have an index
on `t(a)`. We'd like to write the following

```sql
SELECT * FROM t WHERE a = foo
```

However, `a` may be `null`, so `a = foo` may also be null. In Haskell we can at
least lift `==` over `Maybe`, such that `(==) :: Maybe a -> Maybe a -> Bool`,
but in SQL the comparison operator does *not* return `Bool`... it returns `Maybe
Bool`!

This is not necessarily a difference to Opaleye - it just happens that Rel8
provides these operators while no one has yet added them to Opaleye.

The mneumonic is that all operators trailing `.` is replaced with `?`, so `==.`
becomes `==?`, `&&.` becomes `&&?`, and so on.
