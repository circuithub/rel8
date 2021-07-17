# 1.1.0.0 (2021-07-16)

## New features

* You can derive `Rel8able` for "vanilla" higher-kinded data types - data types that don't use the `Column` type family. For example, the following is now possible:

  ```haskell
  data MyTable f = MyTable
    { myTableX :: f Int32
    , myTableY :: f Text
    } deriving (Generic, Rel8able)
  ```

* The `Projectable` and `Biprojectable` type classes have been introduced. These classes model a type of functors and bifunctors (respectively) on tables, where the mapping function is only able to rearrange columns. For example, the following is now possible:

  ```haskell
  x <- project myTableX <$> many ...
  ```

* `Insert`'s `onConflict` field now has a new option - `DoUpdate`. This maps to PostgreSQL's `ON CONFLICT DO UPDATE` syntax, which provides upsert support. ([#85](https://github.com/circuithub/rel8/pull/85))

* `Insert`, `Update` and `Delete` has all been expanded to work with `Query`s. In particular:

  * `Delete` now supports a `USING` sub-query
  * `Update` now supports a `FROM` sub-query
  * `Insert` can delete the result of any arbitrary `Query` (providing the types match)

* The new `Rel8.Tabulate` module has been added, which gives a `Map`-esque interface to writing and composing queries. ([#70](https://github.com/circuithub/rel8/pull/70))

* The new `indexed` `Query -> Query` function was added. This function augments each row in a query with it's 0-based index. ([#117](https://github.com/circuithub/rel8/pull/117))

## Breaking changes

* `Insert`, `Delete` and `Update` have all changed. In particular, for `Insert` users should now replace `rows = xs` with `rows = values xs`. ([#85](https://github.com/circuithub/rel8/pull/85))

* Rename `whereExists` and `whereNotExists` to `present` and `absent` respectively. ([#57](https://github.com/circuithub/rel8/pull/57))

* Simplify `evaluate` to run directly inside the `Query` monad, rendering the `Evaluate` monad unnecessary. ([#56](https://github.com/circuithub/rel8/pull/56))

* The `Labelable` type class has been removed as it's no longer necessary. This class was mostly internal, though was visible in some public API functions. ([#76](https://github.com/circuithub/rel8/pull/76))

* `EitherTable`, `ListTable`, `MaybeTable`, `NonEmptyTable` and `TheseTable` now all have an extra parameter which tracks the context of the table. If you manually specified any types before, you need to provide this parameter - usually `Expr` will be the correct choice. ([#101](https://github.com/circuithub/rel8/pull/101))

* We've done a lot of work to simplify the internals of Rel8, but some of these are breaking changes. However, most of these API changes shouldn't affect users as they are mostly types used in type inference, or affect classes where users don't need to define their own instances.

  * The kind of a `Rel8able` is now `(Type -> Type) -> Type`. Previously it was `(X -> Type) -> Type`.
  * `Table` has a new associated type - `FromExprs`. This was previously an open type family.
  * `Table` has a new associated type - `Transpose` - and `Recontextualise` has been renamed to `Transposes`. This `Transposes` class now operates in terms of `Transpose`.

* `select`, `insert`, `update` and `delete` now produce Hasql `Statement`s, rather than actually running the statement as IO. This allows Rel8 to be used with transaction/connection-managing monads like [`hasql-transaction`](https://hackage.haskell.org/package/hasql-transaction). ([#94](https://github.com/circuithub/rel8/pull/94))

## Bug fixes

* Fixes a bug where cartesian products of queries using `catListTable`, `catNonEmptyTable`, `catList` and `catNonEmpty` would incorrectly be zipped instead. ([#61](https://github.com/circuithub/rel8/pull/61))

* Require Opaleye 0.7.3.0. This version has better support for lateral queries, which can improve query plans, especially in `optional`/`LEFT JOIN` ([#72](https://github.com/circuithub/rel8/pull/72))

* Rel8 now compiles with GHC 9. ([#97](https://github.com/circuithub/rel8/pull/97))

## Other changes

* `exists` is now implemented in terms of the SQL `EXISTS` keyword. ([#69](https://github.com/circuithub/rel8/pull/69))

* `alignBy` no longer requires `Table`s. ([#67](https://github.com/circuithub/rel8/pull/67))


# 1.0.0.1 (2021-06-21)

This release contains various fixes for documentation.

# 1.0.0.0 (2021-06-18)

* Initial release.
