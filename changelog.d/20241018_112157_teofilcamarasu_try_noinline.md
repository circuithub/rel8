### Added

- Add `NOINLINE` pragmas to `Generic` derived default methods of `Rel8able`. This should speed up
  compilation times. If users wish for these methods to be `INLINE`d, they can override with a
  pragma in their own code.
