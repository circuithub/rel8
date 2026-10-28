{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}

module Rel8.Internal.Generic.Table.ADT.Tag
  ( GTagColumns
  , gtagResult, gisTagResult, gtagExpr, gisTagExpr
  )
where

-- base
import Data.Functor.Identity ( Identity( Identity ) )
import Data.Kind ( Type )
import Prelude

-- rel8
import Rel8.Internal.Expr ( Expr )
import Rel8.Internal.Expr.Eq ( (==.) )
import Rel8.Internal.Expr.Serialize ( litExpr )
import Rel8.Internal.Schema.HTable.Identity ( HIdentity( HIdentity ) )
import Rel8.Internal.Schema.HTable.Label ( HLabel, hlabel, hunlabel )
import qualified Rel8.Internal.Schema.Kind as K
import Rel8.Internal.Schema.Result ( Result )
import Rel8.Internal.Type.Tag ( ADTTagFor, tagValue, isTag )


type GTagColumns :: Type -> K.HTable
type GTagColumns tag = HLabel "tag" (HIdentity tag)


gtagResult :: forall tag name. ADTTagFor tag name => GTagColumns tag Result
gtagResult = hlabel (HIdentity (Identity (tagValue @tag @name)))


gisTagResult :: forall tag name. ADTTagFor tag name => GTagColumns tag Result -> Bool
gisTagResult columns = isTag @tag @name tag
  where
    HIdentity (Identity tag) = hunlabel columns


gtagExpr :: forall tag name. ADTTagFor tag name => GTagColumns tag Expr
gtagExpr = hlabel (HIdentity (litExpr (tagValue @tag @name)))


gisTagExpr :: forall tag name. ADTTagFor tag name => GTagColumns tag Expr -> Expr Bool
gisTagExpr columns = tag ==. litExpr (tagValue @tag @name)
  where
    HIdentity tag = hunlabel columns
