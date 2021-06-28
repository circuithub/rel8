``INSERT``, ``UPDATE`` and ``DELETE``
=====================================

While the majority of Rel8 is about building and executing ``SELECT``
statement, Rel8 also has support for ``INSERT``, ``UPDATE`` and ``DELETE``.
These statements are all executed using the ``insert``, ``update`` and
``delete`` functions, each of which take the ``TableSchema`` of the table to
modify, and a ``Insert``, ``Update`` or ``Delete`` value respectively, which
can be constructed using ``do``-notation.

.. note::

   To use ``do``-notation to construct ``Insert``, ``Update`` and ``Delete``
   statements, you'll need to enable the ``ApplicativeDo`` language extension.

``DELETE``
----------

To perform a ``DELETE`` statement, construct a ``Delete`` value and execute it
using ``delete``. ``Delete`` statements can be built from:

``restrict``
  The ``WHERE`` caluse of the ``DELETE`` statement. This is a function that
  takes a single ``Expr`` table as input.

``returning``
  What to return - see :ref:`returning`.

``UPDATE``
----------

To perform a ``UPDATE`` statement, construct a ``Update`` value and execute it
using ``update``. ``Update`` statements can be built from:

``restrict``
  The ``WHERE`` clause of the ``UPDATE`` statement. This is a function that
  takes a single ``Expr`` table as input.

``set``
  A row to row transformation function, indicating how to update selected rows.
  One way to write this function is to use record update syntax::

    set $ \row -> row { rowName = "new name" }

``returning``
  What to return - see :ref:`returning`.

``INSERT``
----------

To perform a ``INSERT`` statement, construct a ``Insert`` value and execute it
using ``insert``. ``Insert`` statements can be built from:

``rows``
  A ``Query`` that returns some rows to insert. You can construct rows from
  their individual fields::

    rows $ values [ MyTable { myTableA = lit "A", myTableB = lit 42 }

  or you can use ``lit`` on a table value in the ``Result`` context::

    rows $ values [ lit MyTable { myTableA = "A", myTableB = 42 }

``onConflict``
  What should happen if an insert clashes with rows that already exist. This
  corresponds to PostgreSQL's ``ON CONFLICT`` clause. You can specify:

  ``doNothing``
    PostgreSQL should silently drop rows that would conflict, but should still
    insert rows that wouldn't.

  ``doUpdate``
    PostgreSQL should update using any conflicting rows (using the ``Update``
    statement) given to ``doUpdate``.

  In the absence of an ``onConflict`` clause, PostgreSQL will throw an
  exception if you try to insert a row that conflicts with an existing row.

``returning``
  What to return - see :ref:`returning`.

.. _returning:

``RETURNING``
-------------

PostgreSQL has the ability to return extra information after a ``DELETE``,
``INSERT`` or ``UPDATE`` statement by attaching a ``RETURNING`` clause. A common
use of this clause is to return any automatically generated sequence values for
primary key columns. Rel8 supports ``RETURNING`` clauses with the ``returning``
function, to which you give a function that selects the fields you want to be
returned.

For example, if we are inserting orders, we might want the order ids returned::

  insert orderSchema $ do
    rows $ values [order]
    orderIds <- returning orderId
    pure orderIds

Default values
--------------

It is fairly common to define tables with default values. While Rel8 does not
have specific functionality for ``DEFAULT``, there are a few options:

``unsafeDefault``
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Rel8 does not have any special support for ``DEFAULT``. If you need to use
default column values in inserts, you can use ``unsafeDefault`` to
construct the ``DEFAULT`` expression::

  insert orderSchema $ do
    rows $ values [ Order { orderId = unsafeDefault, ... } ]
    orderIds <- returning orderId
    pure orderIds

.. warning::
   As the name suggests, this is an unsafe operation. In particular, Rel8 is not
   able to prove that this column does have a default value, so it may be
   possible to introduce a runtime error. Furthermore, ``DEFAULT`` is fairly
   special in SQL, and cannot be combined like other expressions. For example,
   the innocuous expression::

      unsafeDefault + 1

   will lead to a runtime crash.

Reimplement default values in Rel8
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you only need to access default values in Rel8, another option is to specify
them in Rel8, rather than in your database schema.

.. hint::
   A common default value for primary keys is to use `nextval` to obtain the
   next value from a sequence. This can be done in Rel8 by using the ``nextval``
   function::

     insert orderSchema $ do
       rows $ values [ Order { orderId = nextval "order_id_seq", ... } ]
       orderIds <- returning orderId
       pure orderIds
