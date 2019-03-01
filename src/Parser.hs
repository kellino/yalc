{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric, GADTs #-}

module Parser where

import           Data.Typeable
import           GHC.Generics                   ( Generic )
import           Unbound.Generics.LocallyNameless
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Control.Monad.Combinators.Expr
import           Data.Void
import           Data.List                      ( groupBy, sortBy )
import           Data.Function                  ( on )
import           Data.Char                      ( isUpper
                                                , isLower
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Text.IO                  as TIO
import           Control.Monad.State

import           Syntax

-----------
-- Decls --
-----------


data Decl =
    Sig TName Type
  | Def TName Term
  | FixityDecl FixitySpec
  deriving (Show, Generic, Typeable)

------------
-- Parser --
------------

type Error = ParseErrorBundle Text Void

type Parser = StateT ParseState (Parsec Void Text)

rawData :: Parser [Either (ParseError Text Void) Decl]
rawData = between scn eof (e `sepEndBy` scn)
  where
    e = withRecovery recover (Right <$> decl)
    recover err = Left err <$ manyTill anySingle eol

parseProgram :: Text -> Either Error [Either (ParseError Text Void) Decl]
parseProgram = runParser (evalStateT rawData initParseState) "<file>"

pTest :: (Show a) => Parser a -> Text -> IO ()
pTest p t = do
    let res = runParser (evalStateT p initParseState) "<stdin>" t
    case res of
        Left  err -> putStrLn $ errorBundlePretty err
        Right r   -> TIO.putStrLn . pack . show $ r


-----------
-- Lexer --
-----------

lineCmnt :: Parser ()
lineCmnt = L.skipLineComment "--"

blockCmnt :: Parser ()
blockCmnt = L.skipBlockComment "{-" "-}"

sc :: Parser ()
sc = L.space (void $ oneOf (" \t" :: String)) lineCmnt empty

scn :: Parser ()
scn = L.space (void spaceChar) lineCmnt blockCmnt

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: (Char -> Bool) -> Parser Text
identifier t = (lexeme . try) p
  where
    p = do
        x <- letterChar
        when (t x) (fail "starts with the wrong case")
        xs <- many alphaNumChar
        return . pack $ x : xs

var :: Parser Text
var = identifier isUpper

ty :: Parser Text
ty = identifier isLower


---------------
-- Operators --
---------------

data Assoc = L | R deriving Show

data Fixity =
        Infix Assoc Int
      | Prefix Int
      | Postfix Int
      deriving Show

data FixitySpec = FS { fixity :: Fixity , fixityName :: Text , definition :: Term } deriving Show

newtype ParseState = ParseState { fixities :: [FixitySpec] }

initParseState :: ParseState
initParseState = ParseState []

opChars :: String
opChars = ":!#$%&*+./<=>?@\\^|-~"

operator :: Parser String
operator = some $ oneOf opChars

toParser :: FixitySpec -> Operator Parser Term
toParser (FS fix tok fc) = 
    case fix of
      Infix L _ -> InfixL ((\x y -> App (App fc x) y) <$ symbol tok)
      Infix R _ -> undefined

prec :: FixitySpec -> Int
prec (FS (Infix _ n) _ _) = n
prec _ = 0

mkTable :: [FixitySpec] -> [[Operator Parser Term]]
mkTable ops = map (map toParser) $ groupBy ((==) `on` prec) $ sortBy (flip compare `on` prec) ops


-----------
-- Types --
-----------

tyInt :: Parser Type
tyInt = symbol "Num" >> return TyInt

tyBool :: Parser Type
tyBool = symbol "Bool" >> return TyBool

tyUnit :: Parser Type
tyUnit = symbol "()" >> return TyUnit

typeSig :: Parser Type
typeSig = makeExprParser tyfactors table <?> "type"
  where
    tyfactors = choice [parens typeSig, tyInt, tyBool, tyUnit]
    table     = [[arrow "->" Arr]]
    arrow name f = InfixR (f <$ symbol name)


-----------
-- Terms --
-----------

variable :: Parser Term
variable = Var . t2n <$> var

unit :: Parser Term
unit = symbol "()" >> return Unit

numlit :: Parser Term
numlit = NumLit <$> L.decimal

boollit :: Parser Term
boollit = choice [boolTrue, boolFalse]
  where
    boolTrue  = symbol "true" >> return (BoolLit True)
    boolFalse = symbol "false" >> return (BoolLit False)

lambda :: Parser Term
lambda = do
    symbol "\\"
    var <- var
    symbol "."
    Lam . bind (t2n var, noAnnot) <$> term

factor :: Parser Term
factor = choice [parens term, lambda, unit, numlit, boollit, variable]

app :: Parser Term
app = do
    tms <- some factor
    return $ foldl1 App tms

term :: Parser Term
term = do
    st <- get
    let ops = mkTable (fixities st)
    makeExprParser app ops <?> "expression"

lam :: Text -> Term -> Term
lam x t = Lam (bind (t2n x, noAnnot) t) 

-------------------
-- Toplevel Decs --
-------------------

sig :: Parser Decl
sig = do
    name <- var
    symbol ":"
    Sig (t2n name) <$> typeSig

def :: Parser Decl
def = do
    name <- var
    bnds <- many var
    symbol "="
    body <- term <* scn
    let fun = foldr lam body bnds
    return $ Def (t2n name) fun

op :: Parser Decl
op = undefined

decl :: Parser Decl
decl = choice [try sig, def]
