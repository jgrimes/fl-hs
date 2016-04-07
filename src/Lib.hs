module Lib where
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
--import Text.Peggy

data FLDomain = Atom
              | Functions
              | Sequences
              | TaggedPair
              | Exception
                deriving (Eq, Show)

data FLFunc = FLFunc String
            deriving (Eq, Show)

data FLVal = Var String
           | Num Int
           deriving (Eq, Show)

data FL = Compose FL FL
        | Construction [FL]
        | Application FL FL
        | Sequence [FL]
        | Val FLVal
        | Constant FL
        | Func FLFunc
        deriving (Eq, Show)

--data FLFunc2 = FLFunc2 { execFunc :: FL -> FL }

--concatFLFunc :: FLFunc2 -> FLFunc2 -> FL
--concatFLFunc f g = FL (\a -> [execFunc f a] ++ [g a])

-- aka juxt
--construction2 :: [FLFunc2] -> FLFunc2 
--construction2 fs = foldl concatFLFunc (FLFunc2 id) fs

--flLift :: FL -> FL
--flLift (Func f) = 

--eval :: FL -> FL
--eval (Compose f1 f2) = 

-- flDef = P.LanguageDef {
--     P.reservedOpNames = [":"]
--     , P.commentLine = "--"
--     , P.commentStart = "{-"
--     , P.commentEnd = "-}"
--     , P.nestedComments = False
--     , P.identStart = letter <|> char '_'
--     , P.identLetter = alphaNum <|> char '_'
-- --    , P.opStart
--   }

flfunc = FLFunc <$> (choice (fmap string ["add", "sub"]))

var :: Parser FLVal
var = Var <$> many1 letter

num :: Parser FLVal
num = do
  i <- many1 digit
  return $ Num (read i)

flval = var <|> num

val = Val <$> flval

func :: Parser FLFunc
func = flfunc

-- compose = do
--   f1 <- func
--   char '.'
--   f2 <- func
--   return $ Compose f1 f2

-- Operators
compose = string "∘" >> return Compose
application = string ":" >> return Application
constant = string "~" >> return Constant

construction :: Parser FL
construction = do
  e <- brackets (expr `sepBy` (char ','))
  return $ Construction e

--construction = Construction <$> (char '[' *> (fl `sepBy` (char ',')))

flsequence = Sequence <$> (char '<' *> (fl `sepBy` (char ',')) <* char '>')

fl = (Func <$> func) <|> flsequence <|> construction <|> val

--binary name fun assoc = Infix (do{ reservedOp name; return fun }) assoc

expr :: Parser FL
expr = buildExpressionParser table fl <?> "expression"


table = [
  [Infix application AssocLeft],
  [Infix compose AssocLeft],
  [Prefix constant]
  ]

lexer = P.makeTokenParser emptyDef
brackets = P.brackets lexer

ex1 = "add∘sub"
ex2 = "[add,sub,add]:x"
ex3 = "add:34"
ex4 = "<234,342,432>"
ex5 = "~3"

