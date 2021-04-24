Writing queries with ``Query``
==============================

To fetch data from a database, Rel8 allows you to write ``SELECT`` queries using
the ``Query`` monad. While this monad might look a little different from
ordinary SQL, it is equal in expressivity, but due to Haskell's ``do`` notation
we get to benefit from all the means of abstraction available.

Understanding the ``Query`` monad
---------------------------------

Before we look at special functions for working with ``Query``, we'll first take
a moment to understand how the ``Query`` monad works. First, what does the type
``Query a`` *mean*? To be a ``Query a`` means to be a SQL ``SELECT`` query that
selects rows of type ``a``. Usually ``a`` will be an instance of ``Table Expr``,
such as ``Expr Text``, or maybe ``BlogPostComment Expr``.

As ``Query`` is an instance of ``Monad`` means that we already have three
familiar APIs to work with: ``Functor``, ``Applicative``, and ``Monad``.

``Functor Query``
^^^^^^^^^^^^^^^^^

The ``Functor`` instance gives us access to ``fmap``, and its type is::

  fmap :: (a -> b) -> Query a -> Query b

``fmap`` uniformly transforms rows of one type into rows of another type. In
SQL, this is corresponds to a *projection*. For example, if we have a ``Query
(User Expr)``, we might do ``fmap userId`` to transform this into a ``Query
(Expr UserId)``.

``Applicative Query``
^^^^^^^^^^^^^^^^^^^^^

The ``Applicative`` instance for ``Query`` gives us::

  pure :: a -> Query a
  (<*>) :: Query (a -> b) -> Query a -> Query b

``pure`` constructs a ``Query`` that returns exactly one row - a row containing
the ``a`` that was given. This might seem fairly pointless, but it's an
important ``Query`` when compared with ``(<*>)``. The ``<*>`` combines two
``Query``\s by taking their *cartesian product*, followed by a projection that
combines each row into a new row.

One example of using the ``Applicative`` operators is to combine two ``Query``\s
into a tuple::

  pure (,) <*> queryA <*> queryB

``Monad Query``
^^^^^^^^^^^^^^^

The final type class to discuss is ``Monad Query``. ``Monad Query`` has two
methods::

  return :: a -> Query a
  (>>=) :: Query a -> (a -> Query b) -> Query b

``return`` is the same ``pure``, so we won't discuss this further. The much more
interesting operation is ``>>=`` - commonly referred to as "bind". This operator
allows you to *extend* a ``Query`` with a new query. In SQL this is also similar
to a cartesian product, but uses the ``LATERAL`` modifier to allow the second
query to refer to columns from the first.

This extension operator allows you to expand each row in the first query into
zero, one, or many rows, according to the given function. For example, if we
have a database of orders, we might write::

  getAllOrders >>= \order -> getUserById (orderUserId order)

This ``Query`` will return, for each ``Order``, the ``User`` who placed that
order. In this case, this is a one-to-one relationship, so we get back exactly
as many rows as there are orders.

Going in the other direction, we have::

  getAllUsers >>= \user -> getOrdersForUser (userId user)

This is a different query, as we start by fetching all ``User``\s, and for each
user find all ``Order``\s they have placed. This ``Query`` has a different
cardinality, as we're following a one-to-many relationship: any ``User`` may
have zero, one, or many orders.

Haskell has special syntax for working with monads - ``do`` notation. ``do``
notation allows you to write these queries in a simpler form, where we don't
have to introduce functions. Expanding on the latter query, we could write::

  do user  <- getAllUsers
     order <- getOrdersForUser (user)
     return (user, order)

Now we have a query that, for each ``User``, fetches all orders for that user.
The final ``return`` means that for each ``User`` and ``Order``, we'll return a
single row.

Selecting rows from tables
--------------------------

With the more theoretical side of ``Query`` considered, we can start looking at
the more pragmatic side, and how ``Query`` can express some common SQL idioms.

First, one of the most common operations is to select all rows from a *base
table*. In SQL, this is a ``SELECT * FROM x`` query, and in Rel8 we use ``each``
with a ``TableSchema``.

Limit and offset
----------------

The SQL ``LIMIT`` and ``OFFSET`` keywords are available in Rel8 as ``limit``
and ``offset``. Note that, like SQL, the order of these operations matters.
Usually, the correct thing to do is to first apply an offset with ``offset``,
and then use ``limit`` to limit the number of rows returned::

  limit n . offset m . orderBy anOrdering

These operations are similar to Haskell's ``take`` and ``drop`` operations.

Filtering queries
-----------------

Rel8 offers a few different ways to filter the rows selected by a query.
Perhaps the most familiar operation is to apply a ``WHERE`` clause to a query.
In Rel8, this is done using ``where_``, which takes any ``Expr Bool``, and
returns rows where that ``Expr`` is true. For example, to select all public
blog posts, we could write::

  blogPost <- each blogPostSchema
  where_ $ blogPostIsPublic blogPost

An alternative way to write ``WHERE`` clauses is to use ``filter``. This
operator is similar to the ``guard`` function in ``Control.Monad``, but also
returns the tested row. This allows us to easily chain a filtering operation on
a query. The above query could thus be written as::

  blogPost <- filter blogPostIsPublic =<< each blogPostSchema

``where_`` and ``filter`` allow you to filter rows based on an expression, but
sometimes we want to filter based on another query. For this, Rel8 offers
``whereExists`` and ``whereNotExists``. For example, if all blog posts have a
list of tags, we could use ``whereExists`` to find all blog posts that have been
tagged as "Haskell"::

  blogPost <- each blogPostSchema
  whereExists do
    filter (("Haskell" ==.) . tagName) =<< tagFromBlogPost blogPost

Notice that this example uses ``whereExists`` with a query that itself uses
``filter``. For each blog post, ``whereExists`` causes that row to be selected
only if the associated query finds a tag for that blog post with the ``tagName``
"Haskell".

Like ``filter`` there is also a chaining variant of ``whereExists`` - ``with``.
We could rewrite the above query using ``with`` as::

  haskellBlogPost <-
    each blogPostSchema >>=
    with (filter (("Haskell" ==.) . tagName) <=< tagFromBlogPost)

Inner joins
-----------

Inner joins are SQL queries of the form ``SELECT .. FROM x JOIN y ON ..``. Rel8
doesn't offer a special function for these queries, as the same query can be
expressed by selecting from two tables (this is called taking the *cartesian
product* of two queries) and then filtering the result.

If we wanted to join each blog post with the author of the blog post, we would
write the SQL::

  SELECT * FROM blog_post JOIN author ON author.id = blog_post.id

The alternative way to write this query with ``WHERE`` is::

  SELECT * FROM blog_post, author WHERE author.id = blog_post.id

and this query can be written in Rel8 as::

  blogPost <- each blogPostSchema
  author <- each authorSchema
  where_ $ blogPostAuthorId blogPost ==. authorId author

.. hint::

  A good pattern to adopt is to abstract out these joins as functions. A
  suggested way to write the above would be to extract out an "author for blog
  post" function::

    blogPost <- each blogPostSchema
    author <- authorForBlogPost blogPost

  where::

    authorForBlogPost :: BlogPost Expr -> Query (Author Expr)
    authorForBlogPost blogPost =
      filter ((blogPostAuthorId blogPost ==.) authorId author) =<<
      each authorSchema

  While this is a little more code over all, in our experience this style
  dramatically increases the readabality of queries using joins.

Left (outer) joins with ``optional``
------------------------------------

A left join is like an inner join, but allows for the possibility of the join to
"fail". You use left joins when you want to join optional information against a
row.

In Rel8, a ``LEFT JOIN`` is introduced by converting an inner join with
``optional``. While this approach might seem a little foreign at first, it has a
strong similarity with the ``Control.Applicative.optional`` function, and allows
you to reuse previous code.

To see an example of this, let's assume that we want to get the latest comment
for each blog post. Not all blog posts are popular though, so some blog posts
might have no comment at all. To write this in Rel8, we could write::

  blogPost <- each blogPostSchema

  latestComment <-
    optional $ limit 1 $
      orderBy (commentCreatedAt >$< desc) $
        commentsForBlogPost blogPost

``optional`` will transform a ``Query a`` into a ``Query (MaybeTable a)``.
``MaybeTable`` is similar to the normal ``Maybe`` data type in Haskell, and
represents the choice between a ``justTable x`` and a ``nothingTable`` (like
``Just x`` and ``Nothing``, respectively). When you execute a query containing
``MaybeTable x`` with ``select``, Rel8 will return ``Maybe x``. ``MaybeTable``
comes with a library of routines, similar to the functions that can be used to
operate on ``Maybe``. For more details, see the API documentation.

.. hint::

  ``optional`` converts an inner join into a ``LEFT JOIN``, but you can also go
  the other way - and turn a ``LEFT JOIN`` back into an inner join! To do this,
  you can use ``catMaybeTable``, which will select only the rows when the left
  join was successful.

Ordering results
----------------

Rel8 supports ordering the results returned by a ``Query``, using SQL's ``ORDER
BY`` syntax. To specify an ordering, you use ``orderBy`` and supply an
appropriate ``Order`` value.

An ``Order`` is built by combining the order of individual columns, each of
which can be either ascending or descending. To order a single column, you
combine ``asc`` or ``desc`` with ``Order``\s *contravariant* interface. For
example, if we have a table with a ``orderId`` column, we can order a ``Query
(Order Expr)`` by ``orderId`` with::

  orderBy (orderId >$< asc)

To order by multiple columns, combine the individual orders with ``Order``\s
``Monoid`` instance. We could extend the above example to order by the order
date first (with the most recent orders first) with::

  orderBy (mconcat [orderDate >$< desc, orderId >$< asc])

Aggregating queries
-------------------

.. todo::

  Write this

Set operations
--------------

.. todo::

  Write this
