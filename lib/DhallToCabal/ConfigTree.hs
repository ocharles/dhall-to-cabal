{-# language DeriveFunctor #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

module DhallToCabal.ConfigTree ( ConfigTree(..), toConfigTree ) where

import Control.Monad
import Data.Semigroup ( Semigroup ( (<>) ) )
import Dhall.Core hiding ( Const )
import Dhall.Optics ( transformMOf )


-- | 'ConfigTree' captures a logic-monad like expansion of the result of
-- Bool-valued expressions.

data ConfigTree cond a
  = Leaf a
  | Branch cond ( ConfigTree cond a ) ( ConfigTree cond a )
  deriving (Functor, Show)

instance Applicative ( ConfigTree cond ) where
  pure = Leaf
  (<*>) = ap

instance Monad ( ConfigTree cond ) where
  return = pure

  Leaf a >>= f = f a
  Branch cond l r >>= f = Branch cond ( l >>= f ) ( r >>= f )

instance ( Semigroup a ) => Semigroup ( ConfigTree cond a ) where
  (<>) = liftM2 (<>)

instance ( Monoid a ) => Monoid ( ConfigTree cond a ) where
  mempty = pure mempty
  mappend = (<>)


-- | Given a Dhall expression that is of the form @λ( config : Config ) -> a@,
-- find all saturated uses of @config@, and substitute in either @True@ or
-- @False@. The two substitutions are captured in a 'Branch'.

toConfigTree
  :: ( Eq a )
  => Expr s a
  -> ConfigTree ( Expr s a ) ( Expr s a )
toConfigTree e =
  let
    v =
      "config"

    saturated =
      normalize ( App e ( Var v ) )

    loop e =
      case normalize <$> rewriteConfigUse v e of
        Leaf a ->
          Leaf a

        Branch cond l r ->
          Branch cond ( loop =<< l ) ( loop =<< r )

  in loop saturated



-- | Find all config-like uses of a given variable, and expand them into all
-- possible results of evaluation.

rewriteConfigUse :: Var -> Expr s a -> ConfigTree (Expr s a) (Expr s a)
rewriteConfigUse v =
 transformMOf
   subExpressions
   ( \expr ->
       if isConfigUse expr then
         Branch
           expr
             ( pure ( BoolLit True ) )
             ( pure ( BoolLit False ) )
       else
         pure expr
   )

  where

    isConfigUse (App (Field (Var x') "os") _)           | v == x' = True
    isConfigUse (App (Field (Var x') "arch") _)         | v == x' = True
    isConfigUse (App (App (Field (Var x') "impl") _) _) | v == x' = True
    isConfigUse (App (Field (Var x') "flag") _)         | v == x' = True
    isConfigUse _ = False
