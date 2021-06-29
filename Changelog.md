# 1.1.0.0 (??)

## Breaking changes

* Rename `whereExists` and `whereNotExists` to `present` and `absent` respectively. ([#57](https://github.com/circuithub/rel8/pull/57))

* Simplify `evaluate` to run directly inside the `Query` monad, rendering the `Evaluate` monad unnecessary. ([#56](https://github.com/circuithub/rel8/pull/56))

* The `Labelable` type class has been removed as it's no longer necessary. This class was mostly internal, though was visible in some public API functions. ([#76](https://github.com/circuithub/rel8/pull/76))

## Bug fixes

* Fixes a bug where cartesian products of queries using `catListTable`, `catNonEmptyTable`, `catList` and `catNonEmpty` would incorrectly be zipped instead. ([#61](https://github.com/circuithub/rel8/pull/61))

* Require Opaleye 0.7.3.0. This version has better support for lateral queries, which can improve query plans, especially in `optional`/`LEFT JOIN` ([#72](https://github.com/circuithub/rel8/pull/72))

## Other changes

* `exists` is now implemented in terms of the SQL `EXISTS` keyword. ([#69](https://github.com/circuithub/rel8/pull/69))

* `alignBy` no longer requires `Table`s. ([#67](https://github.com/circuithub/rel8/pull/67))


# 1.0.0.1 (2021-06-21)

This release contains various fixes for documentation.

# 1.0.0.0 (2021-06-18)

* Initial release.
