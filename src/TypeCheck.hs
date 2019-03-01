module TypeCheck where

import           Unbound.Generics.LocallyNameless
import           Control.Monad.Except
import           Control.Monad.Reader

import           Syntax
import           Parser
import           Env


annotate :: Decl -> Decl -> TypeM Decl
annotate (Sig n sig) (Def n' def) = if n == n'
    then Def n <$> annotate' sig def
    else throwError ""
    where annotate' :: Type -> Term -> TypeM Term
          annotate' fun@(Arr t1 t2) (Lam bnd) = do
              ((x, _), body) <- unbind bnd
              rest <- annotate' t2 body
              return $ Lam (bind (x, Embed $ Annot $ Just fun) rest)
          annotate' s d = return d

typeOf :: Term -> TypeM (Term, Type)
typeOf n@NumLit{} = return (n, TyInt)

typeOf b@BoolLit{} = return (b, TyBool)

typeOf v@(Var x) = do
    ty <- lookupVar x
    return (v, ty)

typeOf t@(Lam bnd) = do
    ((x, Embed (Annot mt)), body) <- unbind bnd
    case mt of
      Nothing -> throwError "non lambda"
      Just (Arr ty1 t2) -> do
          (_, ty2) <- extend (Sig x ty1) (typeOf body)
          if t2 == ty2 
             then return (t, Arr ty1 ty2)
             else throwError $ "unable to unify " <> show t2 <> " with " <> show ty2

typeOf (App t1 t2) = undefined

typeOf Unit = return (Unit, TyUnit)

runTypeCheck :: Decl -> Decl -> Ctx -> Either String (Decl, Type)
runTypeCheck s d ctx = do  
    t <- runFreshM $ runExceptT $ runReaderT (annotate s d) ctx
    case t of
      def@(Def _ trm) -> do
        (_, ty) <- runFreshM $ runExceptT $ runReaderT (typeOf trm) ctx
        return (def, ty)
