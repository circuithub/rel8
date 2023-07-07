{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Rel8.Column.Null (
  HNull,
)
where

-- base
import Data.Kind (Type)
import Prelude

-- rel8
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Result (Result)
import Rel8.Table.Null (NullTable)


{- | Nest a 'Null' value within a 'Rel8able'. @HNull f a@ will produce a
'NullTable' @a@ in the 'Expr' context, and a @'Maybe' a@ in the 'Result'
context.
-}
type HNull :: K.Context -> Type -> Type
type family HNull context = maybe | maybe -> context where
  HNull Result = Maybe
  HNull context = NullTable context
