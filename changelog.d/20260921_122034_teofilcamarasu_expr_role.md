### Fixed

- Make `Expr` have a representational role, which disallows freely coercing the type within `Expr`. Users can still use `unsafeCastExpr` if they want this behaviour.
