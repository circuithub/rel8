{-# language StandaloneKindSignatures #-}

module Rel8.Query
  ( Query( Query )
  )
where

-- base
import Control.Applicative ( liftA2 )
import Control.Monad ( liftM2 )
import Data.Kind ( Type )
import Data.Monoid ( Any( Any ) )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye
import qualified Opaleye.Internal.QueryArr as Opaleye
import qualified Opaleye.Internal.Tag as Opaleye

-- rel8
import Rel8.Query.Set ( unionAll )
import Rel8.Query.Opaleye ( fromOpaleye )
import Rel8.Query.Values ( values )
import Rel8.Table.Alternative
  ( AltTable, (<|>:)
  , AlternativeTable, emptyTable
  )

-- semigroupoids
import Data.Functor.Apply ( Apply, (<.>) )
import Data.Functor.Bind ( Bind, (>>-) )


-- | The @Query@ monad allows you to compose a @SELECT@ query. This monad has
-- semantics similar to the list (@[]@) monad.
type Query :: Type -> Type
newtype Query a =
  Query (
    -- This is based on Opaleye's Select monad, but with two addtions. We
    -- maintain a stack of PrimExprs from parent previous subselects. In
    -- practice, these are always the results of dummy calls to random().
    --
    -- We also return a Bool that indicates to the parent subselect whether
    -- or not that stack of PrimExprs were used at any point. If they weren't,
    -- then the call to random() is never added to the query.
    --
    -- This is all needed to implement evaluate. Consider the following code:
    --
    -- do
    --   x <- values [lit 'a', lit 'b', lit 'c']
    --   y <- evaluate $ nextval "user_id_seq"
    --   pure (x, y)
    --
    -- If we just used Opaleye's Select monad directly, the SQL would come out
    -- like this:
    --
    -- SELECT
    --   a, b
    -- FROM
    --   (VALUES ('a'), ('b'), ('c')) Q1(a),
    --   LATERAL (SELECT nextval('user_id_seq')) Q2(b);
    --
    -- From the Haskell code, you would intuitively expect to get back the
    -- results of three different calls to nextval(), but from Postgres' point
    -- of view, because the Q2 subquery doesn't reference anything from the Q1
    -- query, it thinks it only needs to call nextval() once. This is actually
    -- exactly the same problem you get with the deprecated ListT IO monad from
    -- the transformers package — *> behaves differently to >>=, so
    -- using ApplicativeDo can change the results of a program. ApplicativeDo
    -- is exactly the optimisation Postgres does on a "LATERAL" query that
    -- doesn't make any references to previous subselects.
    --
    -- Rel8's solution is generate the following SQL instead:
    --
    -- SELECT
    --   a, b
    -- FROM
    --   (SELECT
    --      random() AS dummy,
    --      *
    --    FROM
    --      (VALUES ('a'), ('b'), ('c')) Q1(a)) Q1,
    --   LATERAL (SELECT
    --     CASE
    --       WHEN dummy IS NOT NULL
    --       THEN nextval('user_id_seq')
    --     END) Q2(b);
    --
    -- We use random() here as the dummy value (and not some constant) because
    -- Postgres will again optimize if it sees that a value is constant
    -- (and thus only call nextval() once), but because random() is marked as
    -- VOLATILE, this inhibits Postgres from doing that optimisation.
    --
    -- Why not just reference the a column from the previous query directly
    -- instead of adding a dummy value? Basically, even if we extract out all
    -- the bindings introduced in a PrimQuery, we can't always be sure which
    -- ones refer to constant values, so if we end up laterally referencing a
    -- constant value, then all of this would be for nothing.
    --
    -- Why not just add the call to the previous subselect directly, like so:
    --
    -- SELECT
    --   a, b
    -- FROM
    --   (SELECT
    --      nextval('user_id_seq') AS eval,
    --      *
    --    FROM
    --      (VALUES ('a'), ('b'), ('c')) Q1(a)) Q1,
    --   LATERAL (SELECT eval) Q2(b);
    --
    -- That would work in this case. But consider the following Rel8 code:
    --
    -- do
    --   x <- values [lit 'a', lit 'b', lit 'c']
    --   y <- values [lit 'd', lit 'e', lit 'f']
    --   z <- evaluate $ nextval "user_id_seq"
    --   pure (x, y, z)
    --
    -- How many calls to nextval should there be? Our Haskell intuition says
    -- nine. But that's not what you would get if you used the above
    -- technique. The problem is, which VALUES query should the nextval be
    -- added to? You can choose one or the other to get three calls to
    -- nextval, but you still need to make a superfluous LATERAL references to
    -- the other if you want nine calls. So for the above Rel8 code we generate
    -- the following SQL:
    --
    -- SELECT
    --   a, b, c
    -- FROM
    --   (SELECT
    --      random() AS dummy,
    --      *
    --    FROM
    --      (VALUES ('a'), ('b'), ('c')) Q1(a)) Q1,
    --   (SELECT
    --      random() AS dummy,
    --      *
    --    FROM
    --      (VALUES ('d'), ('e'), ('f')) Q2(b)) Q2,
    --   LATERAL (SELECT
    --     CASE
    --       WHEN Q1.dummy IS NOT NULL AND Q2.dummy IS NOT NULL
    --       THEN nextval('user_id_seq')
    --     END) Q3(c);
    --
    -- This gives nine calls to nextval() as we would expect.
    [Opaleye.PrimExpr] -> Opaleye.Select (Any, a)
  )


instance Functor Query where
  fmap f (Query a) = Query (fmap (fmap (fmap f)) a)


instance Apply Query where
  (<.>) = (<*>)


instance Applicative Query where
  pure = fromOpaleye . pure
  liftA2 = liftM2


instance Bind Query where
  (>>-) = (>>=)


instance Monad Query where
  Query q >>= f = Query $ \dummies -> Opaleye.QueryArr $ \(_, query, tag) ->
    let
      Opaleye.QueryArr qa = q dummies
      ((m, a), query', tag') = qa ((), query, tag)
      Query q' = f a
      (dummies', lquery, tag'') =
        ( dummy : dummies
        , Opaleye.Rebind True bindings query'
        , Opaleye.next tag'
        )
        where
          (dummy, bindings) = Opaleye.run $ name random
            where
              random = Opaleye.FunExpr "random" []
              name = Opaleye.extractAttr "dummy" tag'
      Opaleye.QueryArr qa' = Opaleye.lateral $ \_ -> q' dummies'
      ((m'@(Any needsDummies), b), rquery, tag''') = qa' ((), Opaleye.Unit, tag'')
      lquery'
        | needsDummies = lquery
        | otherwise = query'
      query'''' = Opaleye.times lquery' rquery
      m'' = m <> m'
    in
      ((m'', b), query'''', tag''')


-- | '<|>:' = 'unionAll'.
instance AltTable Query where
  (<|>:) = unionAll


-- | 'emptyTable' = 'values' @[]@.
instance AlternativeTable Query where
  emptyTable = values []
