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

-- langDef is used by Parsec's built-in lexer generator
langDef :: T.LanguageDef ()
langDef = T.LanguageDef
  { T.commentStart    = "{-"
  , T.commentEnd      = "-}"
  , T.commentLine     = "--"
  , T.nestedComments  = Prelude.True
  , T.identStart      = letter <|> oneOf "_'+!"
  , T.identLetter     = try alphaNum <|> oneOf "_'+!"
  , T.opStart         = oneOf ":!#$%&*./<=>?@\\^|-~;"
  , T.opLetter        = oneOf ":!#$%&*./<=>?@\\^|-~;"
  , T.reservedNames   = ["def", "where", "lib", "export", "uses"]
  , T.reservedOpNames = ["->",";"]
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

cond1 :: Parser (Expr -> Expr -> Expr)
cond1 =
  (symbol "->" >> return (\x y -> Cond $ CondExpr x y))

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
  lexeme $ buildExpressionParser table $
    try (Name <$> name)
    <|> Seq <$> Parser.sequence
    <|> try (Atom <$> atom)
    <|> try (PredicateConstr <$> brackets (brackets (expr `sepBy` char ',')))
    <|> Constr <$> brackets (expr `sepBy` char ',')

-- Part of a hack to avoid refactoring the grammar to not
-- be left recursive. Really should learn how to deal with
-- this in a cleaner way.
-- Could potentially allow parsing things that shouldn't
-- be parsed?
-- TODO Add check that lhs is a CondExpr or CondPath
semi = try (symbol ";" >> return (\x y -> Cond $ CondAlternative x y))

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

envTable = [
  [Infix (ws *> reserved "where" >> return EWhere) AssocLeft]
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
  [Infix (ws >> cond1) AssocLeft],
  [Infix (ws >> semi) AssocLeft]
  ]

-- Using Parsec's built in lexer -- makes parsing operators much easier
lexer = T.makeTokenParser langDef
lexeme = T.lexeme lexer
identifier = T.identifier lexer
reserved = T.reserved lexer
reservedOp = T.reservedOp lexer
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
test10 = "def ! == eq0 -> ~1; *o[id, !osub1] where \
         \ { def eq0 == eq o [id, ~0] ; def sub1 == -o[id, ~1] }"
test11 = "+:1 -> +:1"
tests = [test1,test2,test3,test4,test5,test6,test7, test8, test9]
runTests = mapM (print . parse env "") tests

