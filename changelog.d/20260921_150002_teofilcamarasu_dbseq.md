### Added

- Added a `DBSequence` typeclass that tracks database types that can be used with `nextval`

### Changed
- `nextval` has become polymorphic and it can now generate any `DBSequence` rather than just an `Int64`
