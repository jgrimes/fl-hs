module Parser (
    module Parser
  , module Text.Parsec
) where
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
  , T.identStart      = letter <|> oneOf "_'+*"
  , T.identLetter     = try alphaNum <|> oneOf "_'+"
  , T.opStart         = oneOf ":!#$%&*./<=>?@\\^|-~"
  , T.opLetter        = oneOf ":!#$%&*./<=>?@\\^|-~"
  , T.reservedNames   = ["def", "where", "lib", "export", "uses"]
  , T.reservedOpNames = []
  , T.caseSensitive   = Prelude.True
  }


name =
  Identifier <$> identifier
  -- TODO Implement Operator

atom :: Parser Atom
atom =
  Character <$> (char '\'' *> letter)
  <|> Number <$> (read <$> many1 digit)
  <|> (string "true" *> pure S.True)
  <|> (string "false" *> pure S.False)

str' = char '"' *> many1 letter <* char '"'

sequence :: Parser Sequence
sequence =
  Sequence <$> (char '<' *> (expr `sepBy` char ',') <* char '>')
  <|> Str <$> str'

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

-- env is currently the top level, may refactor env to be in with expr?
env :: Parser Env
env =
  buildExpressionParser envTable $
  Defns <$> braces (many1 defn <* ws)
--  <|> Export <$> (reserved "export" *> parens (name `sepBy` symbol ",")) <*> env
--  <|> Hide <$> (string "hide" *> parens (name `sepBy` symbol ",")) <*> env
--  <|> Lib <$> (string "lib" *> parens str')
--  <|> try (Uses <$> env <* ws <* symbol "uses" <*> env)
--  <|> try (Union <$> env <* ws <* symbol "union" <*> env)
  <|> braces env
--  <|> try (EExpr <$> expr)
--  <|> Defn <$> defn

envTable = [
  [Infix (ws *> reserved "where" >> return EWhere) AssocLeft]
--  , [Infix (ws *> reserved "where" >> return EWhere) AssocLeft]
--  , [Infix (ws *> reserved "where" >> return EWhere) AssocLeft]
           ]

top = TEnv <$> env <|> TDefn <$> defn <|> TExpr <$> expr

defn :: Parser Defn
defn =
  Def <$> (reserved "def" *> name <* ws) <*> (optionMaybe (try patExpr) <* ws) <*> (symbol "==" *> expr)

--topExpr = buildExpressionParser table expr <?> "expression"

table = [
  [Prefix constant],
  [Postfix lift],
  [Infix application AssocLeft],
--  [Infix compose AssocLeft],
  [Infix cond AssocLeft]
  ]



lexer = T.makeTokenParser langDef
lexeme = T.lexeme lexer
identifier = T.identifier lexer
reserved = T.reserved lexer
brackets = T.brackets lexer
braces = T.braces lexer
symbol = T.symbol lexer
parens = T.parens lexer
ws = T.whiteSpace lexer

test1 = "234"
test2 = "'c"
test3 = "34->342"
test4 = "[asdf->123]"
test5 = "[[asdf->123]]"
test6 = "[234]where~2342"
test7 = "def hey hey.34 == asdf"
test8 = "id:3"
test9 = "def o == adder:5 where { def adder == + }"
tests = [test1,test2,test3,test4,test5,test6,test7, test8, test9]
runTests = mapM (print . parse env "") tests
