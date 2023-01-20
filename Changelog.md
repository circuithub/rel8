# 1.4.1.0 (2023-01-19)

## New features

* Rel8 now supports window functions. See the "Window functions" section of the `Rel8` module documentation for more details. ([#182](https://github.com/circuithub/rel8/pull/182))
* `Query` now has `Monoid` and `Semigroup` instances. ([#207](https://github.com/circuithub/rel8/pull/207))
* `createOrReplaceView` has been added (to run `CREATE OR REPLACE VIEW`). ([#209](https://github.com/circuithub/rel8/pull/209) and [#212](https://github.com/circuithub/rel8/pull/212))
* `deriving Rel8able` now supports more polymorphism. ([#215](https://github.com/circuithub/rel8/pull/215))
* Support GHC 9.4 ([#199](https://github.com/circuithub/rel8/pull/199))

## Bug fixes

* Insertion of `DEFAULT` values has been fixed. ([#206](https://github.com/circuithub/rel8/pull/206))
* Avoid some exponential SQL generation in `Rel8.Tabulate.alignWith`. ([#213](https://github.com/circuithub/rel8/pull/213))
* `nextVal` has been fixed to work with case-sensitive sequence names. ([#217](https://github.com/circuithub/rel8/pull/217))

## Other

* Correct the documentation for "Supplying `Rel8able` instances" ([#200](https://github.com/circuithub/rel8/pull/200))
* Removed some redundant internal code ([#202](https://github.com/circuithub/rel8/pull/202))
* Rel8 is now less dependant on the internal Opaleye API. ([#204](https://github.com/circuithub/rel8/pull/204))

# 1.4.0.0 (2022-08-17)

## Breaking changes

* The behavior of `greatest`/`least` has been corrected, and was previously flipped. ([#183](https://github.com/circuithub/rel8/pull/183))

## New features

* `NullTable`/`HNull` have been added. This is an alternative to `MaybeTable` that doesn't use a tag columns. It's less flexible (no `Functor` or `Applicative` instance) and is meaningless when used with a table that has no non-nullable columns (so nesting `NullTable` is redundant). But in situations where the underlying `Table` does have non-nullable columns, it can losslessly converted to and from `MaybeTable`. It is useful for embedding into a base table when you don't want to store the extra tag column in your schema. ([#173](https://github.com/circuithub/rel8/pull/173))
* Add `fromMaybeTable`. ([#179](https://github.com/circuithub/rel8/pull/179))
* Add `alignMaybeTable`. ([#196](https://github.com/circuithub/rel8/pull/196))

## Improvements

* Optimize implementation of `AltTable` for `Tabulation` ([#178](https://github.com/circuithub/rel8/pull/178))

## Other
 
* Documentation improvements for `HADT`. ([#177](https://github.com/circuithub/rel8/pull/177))
* Document example usage of `groupBy`. ([#184](https://github.com/circuithub/rel8/pull/184))
* Build with and require Opaleye >= 0.9.3.3. ([#190](https://github.com/circuithub/rel8/pull/190))
* Build with `hasql` 1.6. ([#195](https://github.com/circuithub/rel8/pull/195))

# 1.3.1.0 (2022-01-20)

## Other

* Rel8 now requires Opaleye >= 0.9.1. ([#165](https://github.com/circuithub/rel8/pull/165))

# 1.3.0.0 (2022-01-31)

## Breaking changes

* `div` and `mod` have been changed to match Haskell semantics. If you need the PostgreSQL `div()` and `mod()` functions, use `quot` and `rem`. While this is not an API change, we feel this is a breaking change in semantics and have bumped the major version number. ([#155](https://github.com/circuithub/rel8/pull/155))

## New features

* `divMod` and `quotRem` functions have been added, matching Haskell's `Prelude` functions. ([#155](https://github.com/circuithub/rel8/pull/155))
* `avg` and `mode` aggregation functions to find the mean value of an expression, or the most common row in a query, respectively. ([#152](https://github.com/circuithub/rel8/pull/152))
* The full `EqTable` and `OrdTable` classes have been exported, allowing for instances to be manually created. ([#157](https://github.com/circuithub/rel8/pull/157))
* Added `like` and `ilike` (for the `LIKE` and `ILIKE` operators). ([#146](https://github.com/circuithub/rel8/pull/146))

## Other

* Rel8 now requires Opaleye 0.9. ([#158](https://github.com/circuithub/rel8/pull/158))
* Rel8's test suite supports Hedgehog 1.1. ([#160](https://github.com/circuithub/rel8/pull/160))
* The documentation for binary operations has been corrected. ([#162](https://github.com/circuithub/rel8/pull/162))

# 1.2.2.0 (2021-11-21)

## Other

* Support GHC 9.2 ([#145](https://github.com/circuithub/rel8/pull/145))
* Correct the documentation for `except` ([#147](https://github.com/circuithub/rel8/pull/147))
* Support `hasql` 1.5 ([#149](https://github.com/circuithub/rel8/pull/149))

# 1.2.1.0 (2021-11-21)

## New features

* `castTable` has been added, which casts all columns selected from in `Query` according to the types of columns. This can occasionally be useful when using `many`/`some` on older PostgreSQL versions. ([#137](https://github.com/circuithub/rel8/pull/137))

* Added `<:`, `<=:`, `>:`, `>=:`, `greatest` and `least`, which sort all columns in a table lexicographically. These operators are like the `<.` operators which operate on `Expr`s, but the `<:` operate on entire tables. ([#139](https://github.com/circuithub/rel8/pull/139))

## Other

* Support opaleye-0.8 ([#142](https://github.com/circuithub/rel8/pull/142))

# 1.2.0.0 (2021-10-22)

## New features

* New `$*` and `$+` operators for projecting out of `ListTable` and `NonEmptyTable` respectively (analogous to the existing `$?` for `MaybeTable`). ([#125](https://github.com/circuithub/rel8/pull/125))

## Bug fixes

* Fix `UPSERT` support (and add tests), which was broken due to a typo inside Rel8 that made it impossible to construct `Projection`s which are needed by `UPSERT`. ([#134](https://github.com/circuithub/rel8/pull/134))

* Remove `DBMin` and `DBMax` instances for `Bool`, which are invalid as far as Postgres is concerned. `and` and `or` can be used to achieve the same thing. ([#126](https://github.com/circuithub/rel8/pull/126))

* Fix `aggregateMaybeTable`, `aggregateEitherTable` and `aggregateTheseTable`, which generated invalid SQL previously. ([#127](https://github.com/circuithub/rel8/pull/127))

## Breaking changes

* `rebind` now takes an additional argument for the "name" of the binding. ([#128](https://github.com/circuithub/rel8/pull/128))

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
