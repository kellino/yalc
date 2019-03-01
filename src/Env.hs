{-# LANGUAGE FlexibleContexts #-}

module Env where

import           Unbound.Generics.LocallyNameless
import           Control.Monad.Reader
import           Control.Monad.Except
import           Data.Maybe

import           Syntax
import           Parser


type Ctx = [Decl]

emptyCtx :: Ctx
emptyCtx = []

type TypeM = ReaderT Ctx (ExceptT String FreshM)

lookupVar :: TName -> TypeM Type
lookupVar x = do
    ctx <- ask
    let r = listToMaybe [ ty | Sig x' ty <- ctx, x == x' ]
    case r of
        Nothing -> throwError "not in scope"
        Just ty -> return ty

extend :: (MonadReader Ctx m) => Decl -> m a -> m a
extend d = local (\ds -> d : ds)
