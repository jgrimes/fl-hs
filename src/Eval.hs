module Eval where
import Parser
import qualified Syntax as S
import Syntax hiding (Seq, True, False)
import Text.Parsec
import qualified Data.Map.Strict as Map
import Prelude hiding (seq)
import Data.List as L
import Debug.Trace

-- Glossing over infinite computations in representing the semantic domain
type History = [D_Plus] -- technically should never be empty?

-- D aka D_FL aka semantic domain for FL programs
data D
  = D D_Plus
  | Exception E_FL
  deriving (Eq, Show)

-- DH aka histories
type DH = (D, History)

-- TODO Don't use syntactic atoms for domain atoms? Doesn't especially matter
-- in the current implementation since they are the same.
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

traceOn = False

-- second order prims
prims' :: Map.Map Name D
prims' = Map.mapKeys Identifier $
  Map.mapWithKey (\key val -> (D . Function2 . trace' key) val) $ Map.fromList
  [
    ("K", k) -- const:x:_ = x
  , ("comp", comp)
  , ("map", map')
  , ("lift", lift')
  , ("cons", cons')
  , ("C", c')
  , ("cond", cond')
  ]
  where
    -- I have tried to implement these as closely to the paper as possible
    -- but I think they can be cleaned up significantly
    k x (y,h) = (D x, h)
    comp (Seq fs) (x, h) = foldl (\b (Function f) -> f b) (x, h) (reverse fs)
    comp x1 x2 = error $ "comp " ++ show x1 ++ " - " ++ show x2
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
            ith i = apply (f i) (D x, snd $ ith (i - 1))
            ys = map ((\(D d) -> d) . fst . ith) [1..n]
            hn = snd $ ith n
    -- Currying function -- seems to only work on functions that take a sequence
    c' (Function f) (D y, h) = (D (Function (\(D d,h') -> f (append y d, h'))), h)
      where append item x@(A _) = D $ Seq (item:x:[])
            append _    o = error $ "append: " ++ show o
    cond' (Seq (f1:f2:f3:_)) (D x, h) =
      case y1 of
        (Exception _)   -> (y1, h1)
        (D (A S.False)) -> ap f3 (x, h1)
        _               -> ap f2 (x, h1)
        where (y1, h1) = ap f1 (x, h)
    -- lol tracing
    trace' name f x1 x2 =
      if traceOn
      then trace ("name - " ++ name ++ " x1 " ++ show x1 ++ " x2 " ++ show x2) (f x1 x2)
      else f x1 x2
           
anyExceptions = any (\x -> case x of Exception _ -> True; _ -> False)

boolToFL :: Bool -> S.Atom
boolToFL True = S.True
boolToFL False = S.False

prims :: Map.Map Name D --(DH -> DH)
prims = Map.mapKeys Identifier $ Map.map (D. Function) $ Map.fromList
  [
    ("apply", apply')
  , ("id", id)
  , ("+", plus')
  , ("*", product')
  , ("=", eq')
  , ("intsto", intsto')
  ]
  where apply' (D (Seq (f:x:_)), h) = ap f (x, h)
        plus' = numSeqFn sum
        product' = numSeqFn product
        eq' (D (Seq (x1:x2:[])), h) = (D $ A $ boolToFL (x1 == x2), h)
        eq arg = error $ "eq pattern match error '" ++ show arg
        intsto' (D (A (Number n)), h) = (D $ Seq $ map (A . Number) [1..n], h)
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

-- lifts a D_Plus function application into DD
-- not super necessary but cleans up some often repeated code
ap :: D_Plus -> (D_Plus, History) -> DH
ap f (x, h) = apply (D f) (D x, h)

getPrim n v = let (Just o) = Map.lookup n v in o
apply' = getPrim "apply"

condPrim = getPrim (Identifier "cond")

flName = Name . Identifier
k' = flName "K"
cons' = flName "cons"
lift' = flName "lift"
cond' = flName "cond"

-- mu computes the meaning denoted by an expression
mu :: Expr -> Assignments -> [D_Plus] -> DH
mu (S.Atom a) _ h = (D $ A a, h)
mu (Name n) v h = case Map.lookup n v of
  Nothing  -> error $ "Name: " ++ show n ++ " not found in " ++ show v
  (Just o) -> (o, h)  -- if f is a function name
mu (S.Seq (Sequence xs)) v h = (D $ Seq ys, hs !! n)
  where h0 = h
        n = length xs
        ith 0 = (error "mu - ith", h0)
        ith i = mu (xs!!(i - 1)) v (snd $ ith (i - 1))
        ys = (map ((\(D d) -> d) . fst . ith) [1..n])
        hs = (map (snd . ith) [0..n])
mu (S.Seq (Str s)) v h = (D $ flStr s, h)
mu (Cond (CondAlternative (Cond (CondExpr p x1)) x2)) v h =
  if anyExceptions [D y0, D y1, D y2]
  then (D y2, h2)
  else ap condPrim' (Seq [y0, y1, y2], h)
  where (D y0, h0) = mu p v h
        (D y1, h1) = mu x1 (rho (EExpr p) v -+- v) h0
        (D y2, h2) = mu x2 (rho (EExpr p) v -+- v) h1
        (D condPrim') = condPrim v
--mu (Cond (CondAlternative (Cond (CondExpr p x1)) x2)) v h =
--  mu (Application cond' (S.Seq $ Sequence (p:x1:x2:[]))) v h
mu (Application x1 x2) v h =
  case y1 of
    D _ -> apply y1 (mu x2 v h)
    _   -> (y1, h1)
  where (y1, h1) = mu x1 v h
mu (Constant x) v h = mu (Application k' x) v h
mu (Primed x) v h = mu (Application lift' x) v h
mu (Constr xs) v h = mu (Application cons' (S.Seq $ Sequence xs)) v h
mu (Where x e) v h = mu x v h

dom = Map.keys

-- union of v1 and v2 with clashes resolved in favor of v1
(-+-) :: Assignments -> Assignments -> Assignments
(-+-) = Map.union

-- symmetric union of assignments with disjoint domains
(-|-) v1 v2 =
  if null $ L.intersect (dom v1) (dom v2)
    then v1 -+- v2
    else error $ "-|-" ++ show v1 ++ show v2

-- down arrow from the paper aka domain restriction
(-.-) :: Assignments -> Assignments -> Assignments
(-.-) = Map.intersection

-- creates a single assignment: (if f == g then x else #)
(<--) = Map.singleton

-- F(x, V) from the paper
bigF :: Expr -> Assignments -> D
bigF x v = (D $ Function (\(y, h) -> let (x', h') = mu x v h in (apply x' (y, h'))))

rho :: Env -> Assignments -> Assignments
rho (Defn (Def f Nothing e)) v =
  let v'  = f <-- bigF e (v -+- v'') -- tying the knot~!, but surely there is a better way
      v'' = f <-- bigF e (v -+- v')
  in v'
rho _ v = v

eval1 s env' = case parse expr "" s of
  Right e -> mu e as []
  Left err -> error $ show err
 where as = allPrims -+- (rho env' allPrims)

eval expr e = mu expr (allPrims -+- e) []

eq0 = "def eq0 == comp:<=,[id,~0]>"
sub1 = "def sub1 == C:+:-1"
fact2 = "def fact == eq0 -> ~1; comp:<*,[id,comp:<fact,sub1>]>"
