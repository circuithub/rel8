# 1.1.0.0 (??)

* Fixes a bug where cartesian products of queries using `catListTable`, `catNonEmptyTable`, `catList` and `catNonEmpty` would incorrectly be zipped instead.

* Simplify `evaluate` to run directly inside the `Query` monad, rendering the `Evaluate` monad unnecessary.

* Rename `whereExists` and `whereNotExists` to `present` and `absent` respectively.

* Require Opaleye 0.7.3.0. This version has better support for lateral queries, which can improve query plans, especially in `optional`/`LEFT JOIN` (https://github.com/circuithub/rel8/issues/72)

# 1.0.0.1 (2021-06-21)

This release contains various fixes for documentation.

# 1.0.0.0 (2021-06-18)

* Initial release.
