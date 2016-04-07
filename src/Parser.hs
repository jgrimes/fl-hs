module Parser where
import Syntax as S

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef)


langDef :: T.LanguageDef ()
langDef = T.LanguageDef
  { T.commentStart    = "{-"
  , T.commentEnd      = "-}"
  , T.commentLine     = "--"
  , T.nestedComments  = Prelude.True
  , T.identStart      = letter
  , T.identLetter     = alphaNum <|> oneOf "_'+"
  , T.opStart         = oneOf ":!#$%&*./<=>?@\\^|-~"
  , T.opLetter        = oneOf ":!#$%&*./<=>?@\\^|-~"
  , T.reservedNames   = ["def"]
  , T.reservedOpNames = ["where"]
  , T.caseSensitive   = Prelude.True
  }


name =
  Identifier <$> many1 (letter <|> oneOf "_+*#$/")
  -- TODO Implement Operator

atom :: Parser Atom
atom =
  Character <$> (char '\'' *> letter)
  <|> Number <$> (read <$> many1 digit)
  <|> (string "true" *> pure S.True)
  <|> (string "false" *> pure S.False)



sequence :: Parser Sequence
sequence =
  Sequence <$> (char '<' *> (expr `sepBy` char ',') <* char '>')
  <|> Str <$> (char '"' *> many1 letter <* char '"')

application = string ":" >> return Application
constant = string "~" >> return Constant

lift = string "'" >> return Primed

cond :: Parser (Expr -> Expr -> Expr)
cond =
  (string "->" >> return (\x y -> Cond $ CondExpr x y Nothing))

condPat :: Parser Cond
condPat = do
  pat <- pattern
  _ <- string "->"
  e <- expr
  return $ CondPat pat e Nothing

where' :: Parser (Expr -> Expr -> Expr)
where' =
  ws *> symbol "where" >> return Where

patList :: Parser PatList
patList = PatList <$>
  ((patExpr `sepBy` char ',') <* char ',')
  <*> pattern
  <*> ( char ',' *> (patExpr `sepBy` char ','))

patExpr :: Parser PatExpr
patExpr =
  Pat <$> pattern
  <|> Expr <$> expr

pattern :: Parser Pattern
pattern =
  Elementary <$> (name <* char '.') <*> optionMaybe patExpr
  <|> PatConstruction <$> brackets (brackets patList)

expr :: Parser Expr
expr =
  buildExpressionParser table $
    Name <$> name
    <|> try (Cond <$> condPat)
    <|> Seq <$> Parser.sequence
    <|> Atom <$> atom
    <|> try (PredicateConstr <$> brackets (brackets (expr `sepBy` char ',')))
    <|> Constr <$> brackets (expr `sepBy` char ',')

defn :: Parser Defn
defn =
  Def <$> (symbol "def" *> name <* ws) <*> (optionMaybe (try patExpr) <* ws) <*> (symbol "==" *> expr)

--topExpr = buildExpressionParser table expr <?> "expression"

table = [
  [Prefix constant],
  [Postfix lift],
  [Infix application AssocLeft],
--  [Infix compose AssocLeft],
  [Infix cond AssocLeft],
  [Infix where' AssocLeft]
  ]

lexer = T.makeTokenParser langDef
brackets = T.brackets lexer
symbol = T.symbol lexer
ws = T.whiteSpace lexer

test1 = "234"
test2 = "'c"
test3 = "34->342"
test4 = "[asdf->123]"
test5 = "[[asdf->123]]"
test6 = "[234]where~2342"
test7 = "def hey hey.34 == asdf"
test8 = "id:3"
tests = [test1,test2,test3,test4,test5,test6,test7]
runTests = mapM (print . parse expr "") tests
