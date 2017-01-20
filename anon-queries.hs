{-# LANGUAGE Arrows, DataKinds, PartialTypeSignatures, TypeApplications, TypeOperators, OverloadedLabels #-}

import Control.Arrow
import Labels
import Rel8
import Rel8.Anonymous
import qualified Streaming.Prelude as S
import Data.Int

users :: Query (Row ("userId" := Expr Int32, "userName" := Expr String))
users = selectFrom "users"

comments :: Query (Row ("userId" := Expr Int32, "comment" := Expr String))
comments = selectFrom "comments"

-- formattedComments :: QueryArr (Expr String, Expr String) -- Inferred
formattedComments = proc _ -> do
  user <- users -< ()
  comment <- comments -< ()
  where_ -< get #userId user ==. get #userId comment
  returnA -< (get #userName user, get #comment comment)

getComments :: IO _
getComments = S.toList_ $ select testConn formattedComments

testConn = undefined
