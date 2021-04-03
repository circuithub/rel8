``Expr`` and ``Sql``
====================

``Expr``
--------

Now that we've seen how types are bridged between a database and Haskell via
``DBType``, we can start looking at how expressions are modelled. In Rel8, any
expression will be a value of the form ``Expr a``, where ``a`` is the type of
the expression. For example, the SQL expression ``user.id = 42`` is an ``Expr
Bool``, and the subexpressions ``user.id`` and ``42`` might be ``Expr
UserId``\s.

``Expr``\s are usually created by combining other ``Expr``\s together, like
using numeric operations like `+` and `*`, or by quoting Haskell values into
queries with ``lit``. Continuing with the example of ``user.id = 42``, we can
write this in Rel8 as::

  userId ==. lit (UserId 42)

Here it's assumed that a ``userId :: Expr UserId`` expression is already in
scope, and we can compare that against another ``Expr UserId`` using the ``==.``
operator (which is like Haskell's normal ``==`` operator, but lifted to work on
``Expr``). The expression ``lit (UserId 42)`` quotes the Haskell term ``UserId
42`` literally as the SQL expression ``42``. This particular encoding is chosen
because ``UserId`` is simply a ``newtype`` around ``Int64``.

``null``
--------

So far we've only talked about ``DBType``, which represents database types
*excluding* ``null``. Rel8 chooses this encoding because ``null`` is rather
special in SQL, and doesn't really constitute a distinct type. For example,
there is no SQL notion of "stacking" nulls - which is to say a type like ``Maybe
(Maybe UserId))`` has no real analogous type in SQL.

Of course, as ``null`` is pervasive in SQL queries, Rel8 does support ``null`` -
simply wrap up your type in ``Maybe``. ``Nothing`` will be translated as
``null``, and ``Just`` is used to represent non-null values.

Rel8 comes with a set of functions to work with ``null`` values thash should
feel familiar to Haskell programmers:

* ``null`` creates null values (you can also use ``lit Nothing``).
* ``nullify`` turns an ``Expr a`` into a ``Expr (Maybe a)`` (like ``Just``).
* ``nullable`` allows you to eliminate null values, like the ``maybe`` function.
* ``isNull`` and ``isNonNull`` work like ``isNothing`` and ``isJust``,
  respectively.
* ``mapNullable`` is like ``fmap`` for ``Maybe``, and allows you to map over
  non-null values.
* ``liftOpNullable`` is like ``liftA2`` for ``Maybe``, and allows you to combine
  expressions together, given a way to combine non-null expressions.

``SQL`` and null-polymorphic expressions
----------------------------------------

Through the API reference documentation for Rel8, you might encounter the
``Sql`` type class. For example, if we look at the type of ``litExpr``, we
have::

  litExpr :: Sql DBType a => a -> Expr a 

Here ``Sql DBType a`` means that ``a`` can either be literally a type that has
an instance of ``DBType`` (like ``UserId`` or ``Bool``), *or* that same type
wrapped in ``Maybe`` (so ``Maybe UserId`` or ``Maybe Bool``). ``Maybe`` here
encodes the SQL concept of ``null``.

Some functions work regardless of whether or not a value is null, and in these
cases you'll see ``Sql DBType a``. ``Sql`` can be used with any ``DBType``
subtype. For example, the type of ``div`` is::

  div :: Sql DBIntegral a => Expr a -> Expr a -> Expr a 

Which means ``div`` works on any ``DBIntegral a``, including ``Maybe a``.
