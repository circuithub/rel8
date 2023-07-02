``INSERT``, ``UPDATE`` and ``DELETE``
=====================================

While the majority of Rel8 is about building and executing ``SELECT``
statement, Rel8 also has support for ``INSERT``, ``UPDATE`` and ``DELETE``.
These statements are all executed using the ``manipulate`` function, which
takes a ``Insert``, ``Update`` or ``Delete`` value, all of which are records
of parameters.

.. note::

   This part of Rel8's API uses the ``DuplicateRecordFields`` language
   extension. In code that needs to use this API, you should enable the
   ``DisambiguateRecordFields`` language extension, or you may get errors
   about ambiguous field names.

``DELETE``
----------

To perform a ``DELETE`` statement, construct a ``Delete`` value and execute it
using ``delete``. ``Delete`` takes:

``from``
  The ``TableSchema`` for the table to delete rows from.

``using``
  This is a simple ``Query`` that forms the ``USING`` clause of the ``DELETE``
  statement. This can be used to join against other tables, and the results
  can be referenced in the ``deleteWhere`` parameter. For simple ``DELETE``\s
  where you don't need to do this, you can set ``using = pure ()``.

``deleteWhere``
  The ``WHERE`` clause of the ``DELETE`` statement. This is a function that
  takes two inputs: the result of the ``using`` query, and the current value
  of the row.

``returning``
  What to return - see :ref:`returning`.

``UPDATE``
----------

To perform a ``UPDATE`` statement, construct a ``Update`` value and execute it
using ``update``. ``Update`` takes:

``target``
  The ``TableSchema`` for the table to update rows in.

``from``
  This is a simple ``Query`` that forms the ``FROM`` clause of the ``UPDATE``
  statement. This can be used to join against other tables, and the results
  can be referenced in the ``set`` and ``updateWhere`` parameters. For simple
  ``UPDATE``\s where you don't need to do this, you can set ``from = pure ()``.

``set``
  A row to row transformation function, indicating how to update selected rows.
  This function takes rows of the same shape as ``target`` but in the ``Expr``
  context. One way to write this function is to use record update syntax::

    set = \from row -> row { rowName = "new name" }

``updateWhere``
  The ``WHERE`` clause of the ``UPDATE`` statement. This is a function that
  takes two inputs: the result of the ``from`` query, and the current value of
  the row.

``returning``
  What to return - see :ref:`returning`.

``INSERT``
----------

To perform a ``INSERT`` statement, construct a ``Insert`` value and execute it
using ``insert``. ``Insert`` takes:

``into``
  The ``TableSchema`` for the table to insert rows into.

``rows``
  The rows to insert. These are the same as ``into``, but in the ``Expr``
  context. You can construct rows from their individual fields::

    rows = values [ MyTable { myTableA = lit "A", myTableB = lit 42 }

  or you can use ``lit`` on a table value in the ``Result`` context::

    rows = values [ lit MyTable { myTableA = "A", myTableB = 42 }

``onConflict``
  What should happen if an insert clashes with rows that already exist. This
  corresponds to PostgreSQL's ``ON CONFLICT`` clause. You can specify:

  ``Abort``
    PostgreSQL should abort the ``INSERT`` with an exception

  ``DoNothing``
    PostgreSQL should not insert the duplicate rows.

  ``DoUpdate``
    PostgreSQL should instead try to update any existing rows that conflict
    with rows proposed for insertion.

``returning``
  What to return - see :ref:`returning`.

.. _returning:

``RETURNING``
-------------

PostgreSQL has the ability to return extra information after a ``DELETE``,
``INSERT`` or ``UPDATE`` statement by attaching a ``RETURNING`` clause. A common
use of this clause is to return any automatically generated sequence values for
primary key columns. Rel8 supports ``RETURNING`` clauses by filling in the
``returning`` field and specifying a ``Returning``. A ``Returning`` is a row
to row transformation, allowing you to project out a subset of fields.

For example, if we are inserting orders, we might want the order ids returned::

  insert Insert
    { into = orderSchema
    , rows = values [ order ]
    , onConflict = Abort
    , returning = Returning orderId
    }

If we don't want to return anything, we can use ``NoReturning``::

  insert Insert
    { into = orderSchema
    , rows = values [ order ]
    , onConflict = Abort
    , returning = NoReturning
    }

Default values
--------------

It is fairly common to define tables with default values. While Rel8 does not
have specific functionality for ``DEFAULT``, there are a few options:

``unsafeDefault``
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Rel8 does not have any special support for ``DEFAULT``. If you need to use
default column values in inserts, you can use ``unsafeDefault`` to
construct the ``DEFAULT`` expression::

  Insert
    { into = orderSchema
    , rows = values [ Order { orderId = unsafeDefault, ... } ]
    , onConflict = Abort
    , returning = Returning orderId
    }

.. warning::
   As the name suggests, this is an unsafe operation. In particular, Rel8 is not
   able to prove that this column does have a default value, so it may be
   possible to introduce a runtime error. Furthermore, ``DEFAULT`` is fairly
   special in SQL, and cannot be combined like other expressions. For example,
   the innocuous expression::

      unsafeDefault + 1

   will lead to a runtime crash.

.. warning::
   Also note PostgreSQL's syntax rules mean that ``DEFAULT``` can only appear
   in ``INSERT``` expressions whose rows are specified using ``VALUES``. This
   means that the ``rows`` field of your ``Insert`` record doesn't look like
   ``values [..]``, then ``unsafeDefault`` won't work.


Reimplement default values in Rel8
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you only need to access default values in Rel8, another option is to specify
them in Rel8, rather than in your database schema.

.. hint::
   A common default value for primary keys is to use `nextval` to obtain the
   next value from a sequence. This can be done in Rel8 by using the ``nextval``
   function::

     Insert
       { into = orderSchema
       , rows = values [ Order { orderId = nextval "order_id_seq", ... } ]
       , onConflict = Abort
       , returning = Returning orderId
       }
