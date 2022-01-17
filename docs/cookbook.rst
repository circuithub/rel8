Cookbook
========

This cookbook exists to help you easily form Rel8 queries. It's main purpose is
to help those familiar with SQL to translate their queries into Rel8
``Query``\s.

``SELECT * FROM table``
-----------------------

To select from a table, use ``each``.

Inner joins
-----------

To perform an inner join against two queries, use ``where_`` with a join
condition. For example, the following SQL:

.. code-block:: sql

  SELECT * FROM table_a JOIN table_b ON table_a.x = table_b.y

can be written as::

  myQuery = do
    a <- each tableA
    b <- each tableB
    where_ $ tableAX a ==. tableBY b

Left (outer) joins
------------------

A ``LEFT JOIN`` query is performed by using ``optional``.

For example, the query:

.. code-block:: sql

  SELECT * FROM table_a LEFT JOIN table_b ON table_a.x = table_b.y

can be written as::

  myQuery = do
    a <- each tableA

    maybeB <- optional do
      b <- each tableB
      where_ $ tableAX a ==. tableBY b

    return (a, maybeB)

Note that ``maybeB`` here will be a ``MaybeTable``, which is the Rel8
``Query``-equivalent of ``Maybe``. This allows you to observe if a left join
has succeeded or failed.

Ordering results
----------------

A ``Query`` by default has no ordering - just like in SQL. If you rows back in
a certain order, you can use ``orderBy`` with an ``Order``.

For example, the query:

.. code-block:: sql

  SELECT * FROM my_table ORDER BY my_table.id ASC, my_table.x DESC NULLS FIRST

can be written as::

  myQuery = 
    orderBy (mconcat [ myTableId >$< asc, myTableX >$< nullsFirst desc ]) $
    each myTableSchema

Note that we use ``>$<`` (from ``Data.Functor.Contravariant``) to select table
columns, and we can use ``mconcat`` to combine orderings.

If all columns of a table have an ordering, you can also use ``ascTable`` and
``descTable``. For example::
  
  myQuery = orderBy ascTable $ each myTableSchema

Aggregations
------------

Aggregations in Rel8 work by using ``aggregate``, which takes a ``Query
(Aggregate a)`` and gives you back a ``Query a``.

The following query:

.. code-block:: sql

  SELECT sum(foo), count(distinct bar) FROM table_a

can be written as::

  myQuery = aggregate do
    a <- each tableA
    return $ liftF2 (,) (sum (foo a)) (countDistinct (bar a))

where ``liftF2`` comes from ``Data.Functor.Apply`` from the ``semigroupoids``
library.

Combining aggregations
----------------------

As ``Aggregate`` is an instance of ``Apply`` (which is very similar to
``Applicative``), individual aggregations can be combined. For example, one way
to take the average rating would be to write the query:

.. code-block:: sql

  SELECT sum(rating.score) / count(rating.score) FROM rating

In Rel8, we can write this as::

  myQuery = aggregate do
    rating <- each ratingSchema
    return $ liftF2 (/) (sum (score rating)) (count (score rating))

You can also use ``RebindableSyntax`` and ``ApplicativeDo``::
  
  {-# language ApplicativeDo, RebindableSyntax #-}

  import Data.Functor.Apply ((<.>))

  myQuery = aggregate do
    rating <- each ratingSchema
    return do
      scoreSum        <- sum (score rating)
      numberOfRatings <- count (score rating)
      return (scoreSum / numberOfRatings)
    where (<*>) = (<.>)

For large aggregations, this can often make queries easier to read.

Tree-like queries
-----------------

Rel8 has a fairly unique feature in that it's able to return not just lists of
rows, but can also return *tree*\s.

To understand what this means, we'll consider a small example query for blog
posts. We want our query to return:

1. The latest 5 blog posts that have at least one tag each.
2. For each blog post, all tags.
3. For each blog post, the latest 3 comments if they exist.

In Rel8, we can write this query as::

  latestBlogPosts = do
    post <- each postSchema

    -- Returns a `NonEmptyTable a` which ends up as a `Data.List.NonEmpty a` after the query is run
    tags <- some $ do
      tag <- each tagSchema
      where_ (tagPostId tag ==. postId post)
      return (tagName tag)

    -- Returns a `ListTable a` which ends up as a `[a]` after the query is run
    latestComments <-
      many $
      limit 3 $
      orderBy (commentCreatedAt >$< desc) do
        comment <- each commentSchema
        where_ (commentPostId comment ==. postId post)

    return (post, tags, latestComments)

