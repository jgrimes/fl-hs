module Eval where
import Parser
import qualified Syntax as S
import Syntax hiding (Seq, True, False)
import Text.Parsec
import qualified Data.Map.Strict as Map
import Prelude hiding (seq)

-- Glossing over infinite computations in representing the semantic domain

type History = [D_Plus] -- technically should never be empty?

-- D aka D_FL aka semantic domain for FL programs
data D
  = D D_Plus
  | Exception E_FL
  deriving (Eq, Show)

-- DH aka histories
type DH = (D, History)

-- D_FL+
--type Atoms = [S.Atom]
--type Seqs = [D_Plus]
data Atoms
  = FLInt Int
  | FLFloat Float
  | FLChar Char
  | FLBool Bool
    deriving (Eq, Show)

data D_Plus
  = A S.Atom -- atoms are their value, right now no floats though
  | Seq [D_Plus] -- Seqs(X) = {<x1,...,xn | xi in X ^ n >= 0}
  | Tag D_Plus -- Tag(X) = { ≺ i,x ≻ | i in I ^ x in X }
  | Function (DH -> DH) -- Supposedly "strict and honest" functions
  | Function2 (D_Plus -> DH -> DH) -- 

instance Show D_Plus where
  show (Function _) = "<DH -> DH>"
  show (Function2 _) = "<D_Plus -> DH -> DH>"
  show (A a) = "<Atom: " ++ show a ++ " >"
  show (Tag t) = "<Tag: " ++ show t ++ " >"
  show (Seq s) = "<Seq: " ++ show s ++ " >"

instance Eq D_Plus where
  (A a) == (A a') = a == a'
  (Tag t) == (Tag t') = t == t'
  (Eval.Seq s) == (Eval.Seq s') = s == s'
  _ == _ = Prelude.False

-- strict if f:(e,h) = (e,h) for all e in E_FL
-- honest if when f:(x,h) = (x',h') then h is prefix of h'

-- E_FL aka exception domain
data E_FL
  = Exc D_Plus -- {Exc(x) | x in X}
  deriving (Eq)
    -- can also contain bottom

showAtom (A (Character c)) = [c]
showAtom (A (Number i)) = show i
showAtom _ = error "tried to show non-atom"
concatChars :: [D_Plus] -> String
concatChars = concatMap showAtom

getSeq (Seq plus) = plus


instance Show E_FL where
  show (Exc (Seq (e1:e2:e3:[]))) = "Exc< " ++ gc e1 ++ " , " ++ gc e2 ++ " , " ++ show e3 ++ " >"
    where gc = concatChars . getSeq


type Assignments = Map.Map Name D

exception name typ args = Exception $ (Exc $ (Seq [flStr name, flStr typ, Seq args]))
seq = D . Seq

-- lift haskell function to second order FL function
--so :: DH -> DH -> DH
-- 
-- second order prims
prims' :: Map.Map Name D
prims' = Map.mapKeys Identifier $ Map.map (D . Function2) $ Map.fromList
  [
    ("K", k) -- const:x:_ = x
  , ("comp", comp)
  , ("map", map')
  , ("lift", lift')
  , ("cons", cons')
  ]
  where
    k x (y,h) = (D x, h)
    comp (Seq fs) (x, h) = foldl (\b (Function f) -> f b) (x, h) (reverse fs)
    -- TODO gross
    map' f (D (Seq xs), h0) =
      if anyExceptions evaled
        then (exception "map" "exception" [], hn)
        else (seq (map ((\(D d) -> d) . fst . ith)  [1..n]), hn)
      where n = length xs
            ith 0 = (error "prims' - map' ith 0", h0)
            ith i = apply (D f) (D $ xs !! (i-1), snd $ ith (i - 1))
            evaled = map (fst . ith) [1..n]
            hn = snd $ ith n
    map' f (D y, h) = (exception "map" "arg2" [f, y], h)
    lift' f (D (Seq gs), h0) = out -- lift:f:<g1,...,gn> = f o [g1,..,gn] = o:<f, [g1,..,gn]>
      where (D (Seq gs'), h') = cons' (Seq gs) (D (Seq gs), h0) -- promote seq to cons
            out = comp (Seq (f:gs')) (D (Seq gs'), h')
    cons' (Seq fs) (D x, h0) = (seq ys, hn)
      where n = length fs
            f i = D $ fs !! (i - 1)
            ith 0 = (error "prims' - cons' ith 0", h0)
            ith i = apply (f i) (D $ x, snd $ ith (i - 1))
            ys = map ((\(D d) -> d) . fst . ith) [1..n]
            hn = snd $ ith n

    {- seqFunc f' g (D (Seq xs), h0) =
      if anyExceptions evaled
        then (Exception $ Exc $ flStr "MAPERROR", hn)
        else (D $ Seq (map ((\(D d) -> d) . fst . ith)  [1..n]), hn)
      where n = length xs
            ith 0 = (error "prims' - map' ith 0", h0)
            ith i = apply (D g) (D $ xs !! (i-1), snd $ ith (i - 1))
            evaled = map (fst . ith) [1..n]
            hn = snd $ ith n -}

anyExceptions = any (\x -> case x of Exception _ -> True; _ -> False)

-- lift haskell function to first order FL function
--fo :: DH -> DH
--fo 

prims :: Map.Map Name D --(DH -> DH)
prims = Map.mapKeys Identifier $ Map.map (D. Function) $ Map.fromList
  [
    ("apply", apply')
  , ("id", id)
  , ("+", plus')
  , ("*", product')
  ]
  where apply' (D (Seq (f:x:_)), h) = apply (D f) (D x, h)
        plus' = numSeqFn sum
        product' = numSeqFn product
        numSeqFn f (D (Seq xs), h) = (D$A$ Number $ f (map getNum xs), h)
          where getNum (A (Number n)) = n
                getNum x = error $ show x

allPrims = Map.union prims prims'

flStr :: String -> D_Plus
flStr s = Seq $ map (A . S.Character) s

-- seems like a typo in the paper
-- apply should be as I have it
-- paper has D -> DH -> D but that doesn't make sense in context
apply :: D -> DH -> DH
apply x@(Exception _) (_, h) = (x, h)
apply (D _) (y@(Exception _), h) = (y, h)
apply (D (Function x)) (y, h) = x (y, h)
apply (D (Function2 x)) (D y, h) = (D $ Function x', h)
  where x' = x y -- TODO record history as well? exceptions
apply (D x) (D y, h) = (Exception
                           (Exc
                            (Seq [flStr "apply", flStr "arg1", Seq [x, y]])), h)


apply2 :: D -> DH -> DH
apply2 x@(Exception _) (_, h) = (x, h)
apply2 (D _) (y@(Exception _), h) = (y, h)
apply2 (D (Function2 x)) (D y, h) = (D $ Function x', h)
  where x' = x y -- TODO record history as well? exceptions
apply2 (D x) (D y, h) = (Exception
                           (Exc
                            (Seq [flStr "apply", flStr "arg2", Seq [x, y]])), h)
-- F from the paper
-- f :: Expr -> Assignments
--f x v = apply (D $ Function (\(y, h) -> x')) (y, h')
--  where (x', h') = mu x v h

getPrim n v = let (Just o) = Map.lookup n v in o
apply' = getPrim "apply"

k' = Name $ Identifier "K"
cons' = Name $ Identifier "cons"
lift' = Name $ Identifier "lift"

-- mu computes the meaning denoted by an expression
mu :: Expr -> Assignments -> [D_Plus] -> DH
mu (S.Atom a) _ h = (D $ A a, h)
mu (Name n) v h = let (Just o) = Map.lookup n v in (o, h)  -- if f is a function name
mu (S.Seq (Sequence xs)) v h = (D $ Seq ys, hs !! n)
  where h0 = h
        n = length xs
        ith 0 = (error "mu - ith", h0)
        ith i = mu (xs!!(i - 1)) v (snd $ ith (i - 1))
        ys = (map ((\(D d) -> d) . fst) (map ith [1..n]))
        hs = (map snd (map ith [0..n]))
mu (S.Seq (Str s)) v h = (D $ flStr s, h)
mu (Application x1 x2) v h =
  case y1 of
    D _ -> apply y1 (mu x2 v h)
    _   -> (y1, h1)
  where (y1, h1) = mu x1 v h
mu (Constant x) v h = mu (Application k' x) v h
mu (Primed x) v h = mu (Application lift' x) v h
mu (Constr xs) v h = mu (Application cons' (S.Seq $ Sequence xs)) v h
mu (Where x e) v h = mu x v h
--mu (Constant x) v h = mu (apply (D $ Function2 k) x) v h

k :: D_Plus -> DH -> DH
k x (y,h) = (D x, h)

rho = error "rho not implemented"
--mu (Application x1 x2) v h = mu (apply (apply' v) (Eval.Seq [x1, x2])) v h
--mu 

--mu :: [Expr] -> [Assignment] -> D_Plus -> DH
--mu (S.Atom a) v h = case x of
--  S.Atom a -> undefined
--  _ -> Nothing

-- rho computes the assignment denoted by an expression
--rho :: Expr -> Assignments -> Name -> Either Assignments NoBinding
--rho = undefined

eval s = case parse expr "" s of
  Right e -> mu e as []
  Left err -> error $ show err
 where as = allPrims
