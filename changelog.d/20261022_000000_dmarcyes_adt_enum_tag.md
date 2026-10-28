### Added

- An `ADTWith tag t` type which generalizes the `ADT` type's hardcoded `text`
  tag column, so the tag can be any type, such as a PostgreSQL `enum`.
- An `ADTTag` class to define tag types. A tag type must be a `NonNull` `DBType`
  with `Eq` and `DBEq`, whose constructors are all nullary (zero parameters) and
  correspond one-to-one with the ADT's constructors. This tag <-> ADT bijection
  is defined by `ADTTag`'s associated `TagConstructor` type. Mismatches are
  reported as type errors.
- Generalized `...With` versions of the existing ADT types and functions, such
  as `HADTWith`, `NameADTWith`/`nameADTWith`, `buildADTWith` and
  `constructADTWith`.

### Changed

- `ADT`, `HADT`, `ADTable`, `BuildableADT`, `ConstructableADT`, `NameADT`, etc.
  are now synonyms for their `...With` versions applied to `Tag`, such as
  `type ADT = ADTWith Tag`. Functions like `buildADT` delegate similarly to
  their `...With` versions. Existing code using them is unaffected, apart from
  the following.
- **Breaking:** Because `ADT` is now a type synonym, instances on it such as
  `instance C (ADT t)` require `FlexibleInstances`. This is enabled by default
  in `GHC2021`.
