{-# LANGUAGE DeriveDataTypeable , DeriveGeneric, MultiParamTypeClasses #-}

module Syntax where

import           Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Name
import           Data.Typeable
import           GHC.Generics
import Data.Text

-----------
-- Types --
-----------

data Type =
        TyInt
      | TyBool
      | TyUnit
      | Arr Type Type
      deriving (Show, Eq, Typeable, Generic)


-----------
-- Terms --
-----------

newtype Annot = Annot (Maybe Type)
    deriving (Show, Typeable, Generic)

noAnnot :: Embed Annot
noAnnot = Embed $ Annot Nothing

type TName = Name Term

data Term =
        NumLit Int
      | BoolLit Bool
      | Var TName
      | Lam (Bind (TName, Embed Annot) Term)
      | App Term Term
      | Unit
      deriving (Show, Typeable, Generic)



---------------------------
-- Alpha, Subst and Beta --
---------------------------
    
instance Alpha Type
instance Alpha Term 
instance Alpha Annot

instance Subst Term Type 
instance Subst Term Annot

instance Subst Term Term where
    isvar (Var x) = Just (SubstName x)
    isvar _ = Nothing


-------------
-- Helpers --
-------------

text2Name :: Text -> Name a
text2Name t = makeName (unpack t) 0

t2n :: Text -> Name a
t2n = text2Name

name2Text :: Name a -> Text
name2Text (Fn s _) = pack s
name2Text Bn{} = error "Internal Error: cannot call name2Text for bound names"
