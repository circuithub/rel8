# 1.1.0.0 (??)

* Fixes a bug where cartesian products of queries using `catListTable`, `catNonEmptyTable`, `catList` and `catNonEmpty` would incorrectly be zipped instead.

* Simplify `evaluate` to run directly inside the `Query` monad, rendering the `Evaluate` monad unnecessary.

* Rename `whereExists` and `whereNotExists` to `present` and `absent` respectively.

# 1.0.0.1 (2021-06-21)

This release contains various fixes for documentation.

# 1.0.0.0 (2021-06-18)

* Initial release.
