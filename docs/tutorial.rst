.. highlight:: haskell

Getting Started
===============

In this article, we'll take a tour through the basics of Rel8. We'll learn how
to define base tables, write simple queries, and execute these queries against a
real database.


Required language extensions and imports
----------------------------------------

::

  {-# LANGUAGE Arrows, DataKinds, DeriveGeneric, FlexibleInstances,
               MultiParamTypeClasses, OverloadedStrings #- }

  import Control.Applicative
  import Control.Arrow
  import Rel8

To use Rel8, you will need a few language extensions:

* ``Arrows`` is necessary to use ``proc`` notation - similar to ``do`` notation
  for monads. As with Opaleye, Rel8 uses arrows to guarantee queries are valid.

* ``DataKinds`` is used to promote values to the type level when defining
  table/column metadata.

* ``DeriveGeneric`` is used to automatically derive functions from schema
  information.

The other extensions can be considered as "necessary evil" to provide the type
system extensions needed by Rel8.


Defining base tables
--------------------

In order to query a database of existing tables, we need to let Rel8 know
about these tables, and the schema for each table. This is done by defining a
Haskell *record* for each table in the database. These records should have a
type of the form ``C f name hasDefault t``. Let's see how that looks with some
example tables::

  data Part f = Part
    { partId     :: C f "PID" 'HasDefault Int
    , partName   :: C f "PName" 'NoDefault String
    , partColor  :: C f "Color" 'NoDefault Int
    , partWeight :: C f "Weight" 'NoDefault Double
    , partCity   :: C f "City" 'NoDefault String
    } deriving (Generic)

  instance BaseTable Part where tableName = "part"
  instance Table (Part Expr) (Part QueryResult)

The ``Part`` table has 5 columns, each defined with the ``C f ..`` pattern. For
each column, we are specifying:

1. The column name.
2. Whether or not this column has a default value when inserting new rows. In
   this case ``partId`` does, as this is an auto-incremented primary key managed
   by the database.
3. The type of the column.

After defining the table, we finally need to make instances of ``BaseTable`` and
``Table`` so Rel8 can query this table. By using ``deriving (Generic)``, we
simply need to write ``instance BaseTable Part where tableName = "part"``. The
``Table`` instance demonstrates that a ``Part Expr`` value can be selected from
the database as ``Part QueryResult``.


Querying tables
---------------

With tables defined, we are now ready to write some queries. All ``BaseTable`` s
give rise to a query - the query of all rows in that table::

  allParts :: O.Query (Part Expr)
  allParts = queryTable

Notice the type of ``allParts`` specifies that we're working with ``Part Expr``.
This means that the contents of the ``Part`` record will contain expressions -
one for each column in the table. As ``O.Query`` is a ``Functor``, we can derive
a new query for all part cities in the database::

  allPartCities :: O.Query (Expr String)
  allPartCities = partCity <$> allParts

Now we have a query containing just one column - expressions of type ``String``.

``WHERE`` clauses
-----------------

Usually when we are querying database, we are querying for subsets of
information. In SQL, we apply predicates using ``WHERE`` - and Rel8 supports
this too, in two forms.

Firstly, we can use ``filterQuery``, similar to how we would use ``filter``::

  londonParts :: O.Query (Part Expr)
  londonParts = filterQuery (\p -> partCity p ==. "London") allParts

``filterQuery`` takes a function from rows in a query to a predicate. In this
case we can use ``==.`` to compare to expressions for equality. On the left,
``partCity p :: Expr String``, and on the right ``"London" :: Expr String``
(the literal string ``London``).

Alternatively, we can use ``where_`` with arrow notation, which is similar to
using ``guard`` with ``MonadPlus``::

  heavyParts :: O.Query (Part Expr)
  heavyParts = proc _ -> do
    part <- queryTable -< ()
    where_ -< partWeight part >. 5
    returnA -< part

Joining Queries
---------------

Rel8 supports joining multiple queries into one, in a few different ways.

Products and Inner Joins
^^^^^^^^^^^^^^^^^^^^^^^^

We can take the product of two queries - each row of the first query paired with
each row of the second query - by sequencing queries inside a ``O.Query``. Let's
introduce another table::

  data Supplier f = Supplier
    { supplierId :: C f "SID" 'HasDefault Int
    , supplierName :: C f "SName" 'NoDefault String
    , supplierStatus :: C f "Status" 'NoDefault Int
    , supplierCity :: C f "City" 'NoDefault String
    } deriving (Generic)

  instance BaseTable Supplier where tableName = "supplier"
  instance Table (Supplier Expr) (Supplier QueryResult)

We can take the product of all parts paired against all suppliers by simplying
selecting from both tables and returning a tuple::

  allPartsAndSuppliers :: O.Query (Part Expr, Supplier Expr)
  allPartsAndSuppliers = proc _ -> do
    part <- queryTable -< ()
    supplier <- queryTable -< ()
    returnA -< (part, supplier)

We could write this a little more succinctly using using the ``Applicative``
instance for ``O.Query``, as ``<*>`` corresponds to products::

  allPartsAndSuppliers2 :: O.Query (Part Expr, Supplier Expr)
  allPartsAndSuppliers2 = liftA2 (,) queryTable queryTable

In both queries, we've used ``queryTable`` to select the necessary rows.
``queryTable`` is overloaded, but by knowing the type of rows to select, it will
change which table it queries from.

We can combine products with the techniques we've just seen in order to produce
the inner join of two tables. For example, here is a query to pair up each part
with all suppliers in the same city::

  partsAndSuppliers :: Query (Part Expr, Supplier Expr)
  partsAndSuppliers =
    filterQuery
      (\(part, supplier) -> partCity part ==. supplierCity supplier)
      allPartsAndSuppliers

Left Joins
^^^^^^^^^^

The previous query gave us parts with /at least one/ supplier in the same city.
If a part has no suppliers in the same city, it will be omitted from the
results. But what if we needed this information? In SQL we can capture this with
a ``LEFT JOIN``, and Rel8 supports this.

Left joins can be introduced with the ``leftJoin``, which takes two queries, or
using arrow notation with ``leftJoinA``. Let's look at the latter, as it is
often more concise::

  partsAndSuppliersLJ :: Query (Part Expr, MaybeTable (Supplier Expr))
  partsAndSuppliersLJ = proc _ -> do
    part <- queryTable -< ()
    maybeSupplier
      <- leftJoinA queryTable
      -< \supplier -> partCity part ==. supplierCity supplier
    returnA -< (part, maybeSupplier)

This is a little different to anything we've seen so far, so let's break it
down. ``leftJoinA`` takes as its first argument the query to join in. In this
case we just use ``queryTable`` to select all supplier rows. ``LEFT JOIN`` s also
require a predicate, and we supply this as *input* to ``leftJoinA``. The input
is itself a function, a function from rows in the to-be-joined table to
booleans. Notice that in this predicate, we are free to refer to tables and
columns already in the query (as ``partCity part`` is not part of the supplier
table).

Left joins themselves can be filtered, as they are just another query. However,
the results of a left join are wrapped in ``MaybeTable``, which indicates that
*all* of the columns in this table might be ``null``, if the join failed to
match any rows. We can use this information with our ``partsAndSuppliersLJ``
query to find parts where there are no suppliers in the same city::

  partsWithoutSuppliersInCity :: Query (Part Expr)
  partsWithoutSuppliersInCity = proc _ -> do
    (part, maybeSupplier) <- partsAndSuppliersLJ -< ()
    where_ -< isNull (maybeSupplier $? supplierId)
    returnA -< part

We are filtering our query for suppliers where the id is null. Ordinarily this
would be a type error - we declared that ``supplierId`` contains ``Int``, rather
than ``Maybe Int``. However, because suppliers come from a left join, when we
project out from ``MaybeTable`` *all* columns become nullable. It may help to
think of ``($?)`` as having the type:::

  ($?) :: (a -> Expr b) -> MaybeTable a -> Expr (Maybe b)

though in Rel8 we're a little bit more general.


Aggregation
-----------

To aggregate a series of rows, use the ``aggregate`` query transform.
``aggregate`` takes a ``Query`` that returns any ``AggregateTable`` as a result.
``AggregateTable`` s are like ``Tables``, except that all expressions describe a
way to aggregate data. While tuples are instances of ``AggregateTable``, it's
recommended to introduce new data types to represent aggregations for clarity.

As an example of aggregation, let's start with a table modelling all users in
our application::

  data User f = User
    { userId :: Col f "id" 'HasDefault Int64
    , userLastLoggedIn :: Col f "last_logged_in_at" 'NoDefault UTCTime
    , userType :: Col f "user_type" 'NoDefault Text
    } deriving (Generic)

  instance Table (User Expr) (User QueryResult)
  instance BaseTable User where tableName = "users"

We would like to aggregate over this table, grouped by user type, learning how
many users we have and the latest login time in that group. First, let's
introduce a record to easily be able to refer to this data::

  data UserInfo f = UserInfo
    { userCount :: Anon f Int64
    , latestLogin :: Anon f UTCTime
    , uType :: Anon f Text
    } deriving (Generic)

  instance AggregateTable (UserInfo Aggregate) (UserInfo Expr)
  instange Table (UserInfo Expr) (UserInfo QueryResult)

This record is defined in a similar pattern to tables we've seen previously,
but this time we're using the ``Anon`` decorator, rather than ``C``. ``Anon``
can be used for tables that aren't base tables, and means we don't have to
provide metadata about the column name and whether or not it has a default
value. In this case, ``UserInfo`` doesn't model a base table, it models a
derived table.

Also, notice that we derived a new type class instance that we haven't seen yet.
``UserInfo`` will be used with ``Aggregate`` expressions, and the
``AggregateTable`` instance states we can aggregate the ``UserInfo`` data type.

With this, aggregation can be written as a concise query::

  userInfo :: Query (UserInfo Expr)
  userInfo = aggregate $ proc _ -> do
    user <- queryTable -< ()
    returnA -< UserInfo { userCount = count (userId user)
                        , latestLogin = max (userLastLoggedIn user)
                        , uType = groupBy (userType user)
                        }

Running Queries
---------------
