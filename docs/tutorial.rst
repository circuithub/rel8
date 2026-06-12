.. highlight:: haskell

Getting Started
===============

In this section, we'll take a look at using Rel8 to work with a small database
for Haskell packages. We'll take a look at idiomatic usage of Rel8, mapping
tables to Haskell, and then look at writing some simple queries.

Before we get started, we'll be using the following language extensions and
imports throughout this guide::

  {-# language BlockArguments #-}
  {-# language DeriveAnyClass #-}
  {-# language DeriveGeneric #-}
  {-# language DerivingStrategies #-}
  {-# language DerivingVia #-}
  {-# language DuplicateRecordFields #-}
  {-# language GeneralizedNewtypeDeriving #-}
  {-# language OverloadedStrings #-}
  {-# language StandaloneDeriving #-}
  {-# language TypeApplications #-}
  {-# language TypeFamilies #-}

  import Prelude
  import Rel8

The Example Schema
------------------

Before we start writing any Haskell, let's take a look at the schema we'll work
with. The `author` table has three columns:

+-----------------+-------------+----------+
| Column Name     | Type        | Nullable |
+=================+=============+==========+
| ``author_id``   | ``integer`` | not null |
+-----------------+-------------+----------+
| ``name``        | ``text``    | not null |
+-----------------+-------------+----------+
| ``url``         | ``text``    |          |
+-----------------+-------------+----------+

and the `project` table has two:

+-----------------+-------------+----------+
| Column Name     | Type        | Nullable |
+=================+=============+==========+
| ``author_id``   | ``integer`` | not null |
+-----------------+-------------+----------+
| ``name``        | ``text``    | not null |
+-----------------+-------------+----------+

A ``project`` always has an ``author``, but not all ``author``\s have projects.
Each ``author`` has a name and (maybe) an associated website, and each project
has a name.

Mapping Schemas to Haskell
--------------------------

Now that we've seen our schema, we can begin writing a mapping in Rel8. The
idiomatic way to map a table is to use a record that is parameterised what Rel8
calls an *interpretation functor*, and to define each field with ``Column``.
For this type to be usable with Rel8 we need it to be an instance of
``Rel8able``, which can be derived with a combination of ``DeriveAnyClass`` and
``DeriveGeneric`` language extensions.

Following these steps for ``author``, we have::

  data Author f = Author
    { authorId :: Column f Int64
    , name     :: Column f Text
    , url      :: Column f (Maybe Text)
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

This is a perfectly reasonable definition, but cautious readers might notice a
problem - in particular, with the type of the ``authorId`` field.  While
``Int64`` is correct, it's not the best type. If we had other identifier types
in our project, it would be too easy to accidentally mix them up and create
nonsensical joins. As Haskell programmers, we often solve this problem by
creating ``newtype`` wrappers, and we can also use this technique with Rel8::

  newtype AuthorId = AuthorId { toInt64 :: Int64 }
    deriving newtype (DBEq, DBType, Eq, Show)

Now we can write our final schema mapping. First, the ``author`` table::

  data Author f = Author
    { authorId   :: Column f AuthorId
    , authorName :: Column f Text
    , authorUrl  :: Column f (Maybe Text)
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

And similarly, the ``project`` table::

  data Project f = Project
    { projectAuthorId :: Column f AuthorId
    , projectName     :: Column f Text
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

To show query results in this documentation, we'll also need ``Show`` instances:
Unfortunately these definitions look a bit scary, but they are essentially just
``deriving (Show)``::

  deriving stock instance f ~ Result => Show (Author f)
  deriving stock instance f ~ Result => Show (Project f)

These data types describe the structural mapping of the tables, but we also
need to specify a ``TableSchema`` for each table. A ``TableSchema`` contains
the name of the table and the name of all columns in the table, which will
ultimately allow us to ``SELECT`` and ``INSERT`` rows for these tables.

To define a ``TableSchema``, we just need to fill construct appropriate
``TableSchema`` values. When it comes to the ``tableColumns`` field, we
construct values of our data types above, and set each field to the name of the
column that it maps to.

First, ``authorSchema`` describes the column names of the ``author`` table when
associated with the ``Author`` type::

  authorSchema :: TableSchema (Author Name)
  authorSchema = TableSchema
    { name = "author"
    , columns = Author
        { authorId = "author_id"
        , authorName = "name"
        , authorUrl = "url"
        }
    }

And likewise for ``project`` and ``Project``::

  projectSchema :: TableSchema (Project Name)
  projectSchema = TableSchema
    { name = "project"
    , columns = Project
        { projectAuthorId = "author_id"
        , projectName = "name"
        }
    }

There is also some generics machinery available if you want to grab the field
information from your ``Rel8able`` type::

  projectSchema :: TableSchema (Project Name)
  projectSchema = TableSchema
    { name = "project"
    , columns = namesFromLabels @(Project Name)
    }

This will create a ``TableSchema`` for ``Project`` where every column name
corresponds exactly to the name of the field. If you need more flexibility, you
can use ``namesFromLabelsWith``, which takes a transformation function.

.. note::

  You might be wondering why this information isn't in the definitions of
  ``Author`` and ``Project`` above. Rel8 decouples ``TableSchema`` from the data
  types themselves, as not all tables you define will necessarily have a schema.
  For example, Rel8 allows you to define helper types to simplify the types of
  queries - these tables only exist at query time, but there is no corresponding
  base table. We'll see more on this idea later!

With these table definitions, we can now start writing some queries!

Writing Queries
---------------

Simple Queries
~~~~~~~~~~~~~~

First, we'll take a look at ``SELECT`` statements - usually the bulk of most
database heavy applications.

In Rel8, ``SELECT`` statements are built using the ``Query`` monad. You can
think of this monad like the ordinary ``[]`` (List) monad - but this isn't
required knowledge.

To start, we'll look at one of the simplest queries possible - a basic ``SELECT
* FROM`` statement. To select all rows from a table, we use ``each``, and
supply a ``TableSchema``. So to select all ``project`` rows, we can write::

  >>> :t each projectSchema
  each projectSchema :: Query (Project Expr)

Notice that ``each`` gives us a ``Query`` that yields ``Project Expr`` rows. To
see what this means, let's have a look at a single field of a ``Project Expr``::

  >>> let aProjectExpr = undefined :: Project Expr
  >>> :t projectAuthorId aProjectExpr
  projectAuthorId aProjectExpr :: Expr AuthorId

Recall we defined ``projectAuthorId`` as ``Column f AuthorId``. Now we have
``f`` is ``Expr``, and ``Column Expr AuthorId`` reduces to ``Expr AuthorId``.
We'll see more about ``Expr`` soon, but you can think of ``Expr a`` as "SQL
expressions of type ``a``\".

To execute this ``Query``, we pass it to ``select``. This function takes both a
database connection (which can be obtained using hasql's ``acquire`` function),
and the ``Query`` to run::

  >>> Right conn <- acquire "user=postgres"
  >>> :t select conn (each projectSchema)
  select conn (each projectSchema) :: MonadIO m => m [Project Result]

When we ``select`` things containing ``Expr``\s, Rel8 builds a new response
table with the ``Result`` interpretation. This means you'll get back plain
Haskell values. Studying ``projectAuthorId`` again, we have::

  >>> let aProjectResult = undefined :: Project Result
  >>> :t projectAuthorId aProjectResult
  projectAuthorId aProjectResult :: AuthorId

Here ``Column Result AuthorId`` reduces to just ``AuthorId``, with no
wrappping type at all.

Putting this all together, we can run our first query::

  >>> select conn (each projectSchema) >>= mapM_ print
  Project {projectAuthorId = 1, projectName = "rel8"}
  Project {projectAuthorId = 2, projectName = "aeson"}
  Project {projectAuthorId = 2, projectName = "text"}

We now know that ``each`` is the equivalent of a ``SELECT *`` query, but
sometimes we're only interested in a subset of the columns of a table. To
restrict the returned columns, we can specify a projection by using ``Query``\s
``Functor`` instance::

  >>> select conn $ projectName <$> each projectSchema
  ["rel8","aeson","text"]

Joins
~~~~~

Another common operation in relational databases is to take the ``JOIN`` of
multiple tables. Rel8 doesn't have a specific join operation, but we can
recover the functionality of a join by selecting all rows of two tables, and
then using ``where_`` to filter them.

To see how this works, first let's look at taking the product of two tables.
We can do this by simply calling ``each`` twice, and then returning a tuple of
their results::

  >>> :{
  mapM_ print =<< select conn do
    author  <- each authorSchema
    project <- each projectSchema
    return (projectName project, authorName author)
  :}
  ("rel8","Ollie")
  ("rel8","Bryan O'Sullivan")
  ("rel8","Emily Pillmore")
  ("aeson","Ollie")
  ("aeson","Bryan O'Sullivan")
  ("aeson","Emily Pillmore")
  ("text","Ollie")
  ("text","Bryan O'Sullivan")
  ("text","Emily Pillmore")

This isn't quite right, though, as we have ended up pairing up the wrong
projects and authors. To fix this, we can use ``where_`` to restrict the
returned rows. We could write::

  select conn $ do
    author  <- each authorSchema
    project <- each projectSchema
    where_ $ projectAuthorId project ==. authorId author
    return (project, author)

but doing this every time you need a join can obscure the meaning of the
query you're writing. A good practice is to introduce specialised functions
for the particular joins in your database. In our case, this would be::

  projectsForAuthor :: Author Expr -> Query (Project Expr)
  projectsForAuthor a = each projectSchema >>= filter \p ->
    projectAuthorId p ==. authorId a

Our final query is then::

  >>> :{
  mapM_ print =<< select conn do
    author  <- each authorSchema
    project <- projectsForAuthor author
    return (projectName project, authorName author)
  :}
  ("rel8","Ollie")
  ("aeson","Bryan O'Sullivan")
  ("text","Bryan O'Sullivan")

Left Joins
~~~~~~~~~~

Rel8 is also capable of performing ``LEFT JOIN``\s. To perform ``LEFT JOIN``\s,
we follow the same approach as before, but use the ``optional`` query
transformer to allow for the possibility of the join to fail.

In our test database, we can see that there's another author who we haven't
seen yet::

  >>> select conn $ authorName <$> each authorSchema
  ["Ollie","Bryan O'Sullivan","Emily Pillmore"]

Emily wasn't returned in our earlier query because - in our database - she
doesn't have any registered projects. We can account for this partiality in our
original query by wrapping the ``projectsForAuthor`` call with ``optional``::

  >>> :{
  mapM_ print =<< select conn do
    author   <- each authorSchema
    mproject <- optional $ projectsForAuthor author
    return (authorName author, projectName <$> mproject)
  :}
  ("Ollie",Just "rel8")
  ("Bryan O'Sullivan",Just "aeson")
  ("Bryan O'Sullivan",Just "text")
  ("Emily Pillmore",Nothing)


Aggregation
~~~~~~~~~~~

Aggregations are operations like ``sum`` and ``count`` - operations that reduce
multiple rows to single values. To perform aggregations in Rel8, we can use the
``aggregate`` function, which takes a ``Query`` of aggregated expressions, runs
the aggregation, and returns aggregated rows.

To start, let's look at a simple aggregation that tells us how many projects
exist:

.. todo::

  >>> error "TODO"

Rel8 is also capable of aggregating multiple rows into a single row by
concatenating all rows as a list. This aggregation allows us to break free of
the row-orientated nature of SQL and write queries that return tree-like
structures. Earlier we saw an example of returning authors with their projects,
but the query didn't do a great job of describing the one-to-many relationship
between authors and their projects.

Let's look again at a query that returns authors and their projects, and
focus on the /type/ of that query::

  projectsForAuthor a = each projectSchema >>= filter \p ->
    projectAuthorId p ==. authorId a

  let authorsAndProjects = do
        author  <- each authorSchema
        project <- projectsForAuthor author
        return (author, project)

  >>> :t select conn authorsAndProjects
  select conn authorsAndProjects
    :: MonadIO m => m [(Author Result, Project Result)]


Our query gives us a single list of pairs of authors and projects. However,
with our domain knowledge of the schema, this isn't a great type - what we'd
rather have is a list of pairs of authors and /lists/ of projects. That is,
what we'd like is::

  [(Author Result, [Project Result])]

This would be a much better type! Rel8 can produce a query with this type by
simply wrapping the call to ``projectsForAuthor`` with either ``some`` or
``many``.  Here we'll use ``many``, which allows for the possibility of an
author to have no projects::

  >>> :{
  mapM_ print =<< select conn do
    author       <- each authorSchema
    projectNames <- many $ projectName <$> projectsForAuthor author
    return (authorName author, projectNames)
  :}
  ("Ollie",["rel8"])
  ("Bryan O'Sullivan",["aeson","text"])
  ("Emily Pillmore",[])
