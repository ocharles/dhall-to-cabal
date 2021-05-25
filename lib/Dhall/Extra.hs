{-# language ApplicativeDo #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module Dhall.Extra
  ( validateDecoder
  , sortExpr
  )
  where

import qualified Dhall
import qualified Dhall.Core as Dhall ( Expr )
import qualified Dhall.Core as Expr ( Expr(..) )
import qualified Dhall.Map as Map


validateDecoder :: Dhall.Decoder ( Maybe a ) -> Dhall.Decoder a
validateDecoder a =
  a { Dhall.extract =
        \expr ->
          case Dhall.toMonadic (Dhall.extract a expr) of
            Left extractErrors -> Dhall.fromMonadic (Left extractErrors)
            Right Nothing -> Dhall.extractError "Validation failed"
            Right (Just ok) -> pure ok
          }


sortExpr :: Dhall.Expr s a -> Dhall.Expr s a
sortExpr = \case
  Expr.RecordLit r ->
    Expr.RecordLit ( Map.sort r )

  Expr.Record r ->
    Expr.Record ( Map.sort r )

  Expr.Union r ->
    Expr.Union ( Map.sort r )

  e ->
    e
