{-# language DataKinds #-}
{-# language GADTs #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Schema.Result
  ( Col( Result ), Result
  )
where

-- base
import Prelude ()

-- rel8
import Rel8.Schema.Context ( Interpretation( Col ) )
import Rel8.Schema.Kind ( Context )
import Rel8.Schema.Spec ( Spec( Spec ) )


type Result :: Context
data Result a


instance Interpretation Result where
  data Col Result _spec where
    Result :: a -> Col Result ('Spec labels necessity a)
