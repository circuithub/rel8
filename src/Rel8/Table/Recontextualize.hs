{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Table.Recontextualize
  ( Recontextualize
  )
where

-- base
import Data.Functor.Identity ( Identity )
import Data.Kind ( Constraint, Type )
import Prelude ()

-- rel8
import Rel8.Schema.Context ( Col )
import Rel8.Schema.HTable ( HTable )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Null ( Sql )
import Rel8.Schema.Result ( Result )
import Rel8.Schema.Spec ( KnownSpec )
import Rel8.Table ( Table, Congruent )
import Rel8.Type ( DBType )


-- | @Recontextualize from to a b@ is evidence that the types @a@ and @b@ are
-- related, and that @b@ is the same type as @a@, but after changing the
-- context from the initial context @from@, to the new context @to@.
type Recontextualize :: K.Context -> K.Context -> Type -> Type -> Constraint
class
  ( Table from a
  , Table to b
  , Congruent a b
  , Recontextualize from from a a
  , Recontextualize to to b b
  , Recontextualize to from b a
  ) => Recontextualize from to a b
    | a -> from
    , b -> to
    , a to -> b
    , b from -> a


instance Sql DBType a => Recontextualize Result Result (Identity a) (Identity a)


instance KnownSpec spec => Recontextualize from to (Col from spec) (Col to spec)


instance HTable t => Recontextualize from to (t (Col from)) (t (Col to))


instance
  ( Recontextualize from to a1 b1
  , Recontextualize from to a2 b2
  )
  => Recontextualize from to (a1, a2) (b1, b2)


instance
  ( Recontextualize from to a1 b1
  , Recontextualize from to a2 b2
  , Recontextualize from to a3 b3
  )
  => Recontextualize from to (a1, a2, a3) (b1, b2, b3)


instance
  ( Recontextualize from to a1 b1
  , Recontextualize from to a2 b2
  , Recontextualize from to a3 b3
  , Recontextualize from to a4 b4
  )
  => Recontextualize from to (a1, a2, a3, a4) (b1, b2, b3, b4)


instance
  ( Recontextualize from to a1 b1
  , Recontextualize from to a2 b2
  , Recontextualize from to a3 b3
  , Recontextualize from to a4 b4
  , Recontextualize from to a5 b5
  )
  => Recontextualize from to (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5)


instance
  ( Recontextualize from to a1 b1
  , Recontextualize from to a2 b2
  , Recontextualize from to a3 b3
  , Recontextualize from to a4 b4
  , Recontextualize from to a5 b5
  , Recontextualize from to a6 b6
  )
  => Recontextualize from to (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6)


instance
  ( Recontextualize from to a1 b1
  , Recontextualize from to a2 b2
  , Recontextualize from to a3 b3
  , Recontextualize from to a4 b4
  , Recontextualize from to a5 b5
  , Recontextualize from to a6 b6
  , Recontextualize from to a7 b7
  )
  => Recontextualize from to (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7)
