Tables
======

So far we've seen that ``DBType`` bridges Haskell types to database types, and
``Expr`` lets us write SQL expressions using these types. The next concept is to
understand how tables are mapped into Rel8.

In Rel8, we understand a table to be a list of columns. Each column has some
associated metadata - a column has a type, but it also has information as to
whether it can store ``null``, the name of the column, and whether this column
has a default value. All of this metadata can be configured when you define
custom table types, but first we'll look at some built in tables.

``Expr`` is a single-column table
---------------------------------

Any time you have an ``Expr``, you also have a table. All ``Expr``\s are tables
that consist of exactly one column. This means that whenever a table is
expected, you can usually use an ``Expr`` as well.

Tuples combine tables
---------------------

You can use Haskell's normal tuple syntax to combine tables into larger tables.
Now that we know that an ``Expr`` is a table with one column, we can use tuples
to build larger tables. For example, if we have ``userId :: Expr UserId`` and a
``name :: Expr Text``, we can pair these together as ``(userId, name) :: (Expr
UserId, Expr Text)``.

Rel8 supports tuples with two, three, four and five elements. Beyond that, we
suggest writing a custom table type (though you can also nest tuples).

Custom table types
------------------

The primary way to define a table in Rel8 is to use the higher-kinded data
pattern. Rel8 advocates this system because it means you can define your data
type once, but use it in the context of Haskell expressions (for example, to
serialize it as JSON to use as a REST API call response type), and also within
Rel8 queries. This allows developers to share their understanding of a type in
multiple domains, while also reducing the amount of code that has to be written
and maintained.

To define a custom table using this pattern, you define a Haskell data type that
has a single parameter (conventionally called ``f``). Next, we suggest using
record syntax to define the fields of your data type, and for each field use the
``Column`` type family to define the type of the column. Once all fields have
been defined, you can bridge this type with Rel8 by deriving ``Generic`` and
``Rel8able`` instance.

A typical table definition might look like this::

  data User f = User
    { userId :: Column f UserId
    , userName :: Column f Text
    , userCreatedAt :: Column f UTCTime
    , userEmail :: Column f (Maybe EmailAddress)
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)
