module Parser (runParser, lambdaExpr, showLambda, oper) where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

import Control.Monad
import Data.Set
import Data.Map
import Data.Maybe
import Data.List

-- import Control.Monad.Trans (lift)


type Variable  = String
data Lambda  = Variable Variable
             | Abs Variable Lambda
             | Apply Lambda Lambda
--             | Apply Variable Lambda Lambda

               deriving Show

showLambda :: Lambda -> String
showLambda (Variable v) = v
showLambda (Abs var body) =
    "lambda" ++ var ++  " . " ++ showLambda body
showLambda (Apply m n) =
    (showLambda $ m) ++ "(" ++ (showLambda $ n) ++ ")"




power :: [a]  -> [[a]]
power xs = [ [x, y] | x<-xs, y<- tail$xs]



fv :: Lambda -> Set Variable
fv (Variable x) = Data.Set.singleton  x
fv (Apply m n)  = (fv m) `Data.Set.union` (fv n)
fv (Abs x m)    = Data.Set.delete  x (fv m)

{-
eval :: Lambda -> Lambda
eval (Variable a)      = Variable a
eval (Lambda var body) = (Lambda var body)
eval (Apply
-}

alpha :: Variable -> Variable -> Lambda -> Lambda
alpha var var' (Variable v) =
    if var == v then (Variable var') else (Variable v)
alpha var var' (Abs v m) =
    (Abs v' (alpha var var' m))
    where
      v' = if var == v then var' else v
alpha var var' (Apply m1 m2) =
    (Apply (alpha var var' m1) (alpha var var' m2))


recusive_alpha :: Variable -> Lambda -> Lambda
recusive_alpha var m =
    recusive_alpha' (Data.Map.singleton var var) m
    where
      recusive_alpha' varMap (Variable v) =
            Variable (Data.Map.findWithDefault v v varMap)
      recusive_alpha' varMap (Abs v body) =
          recusive_alpha'  varMap' (Abs v' (alpha v v' body))
              where v'      = if v==var then  gensym varSet v else v
                    varMap' = Data.Map.insert v v' varMap
                    varSet  = (Data.Set.fromList . Data.Map.elems $ varMap)
                              `Data.Set.union`
                              (Data.Set.fromList . Data.Map.keys  $ varMap)
      recusive_alpha' varMap (Apply m n) =
          (Apply (recusive_alpha' varMap m)
                 (recusive_alpha' varMap n))




gensym :: Set Variable -> Variable -> Variable
gensym vs v =
    fromJust $ find (not . (`Data.Set.member` vs))
                 [v ++ "_" ++ show i | i <- [1..]]


beta :: Variable -> Lambda -> Lambda -> Lambda
beta  var (Variable v) m =
    if v == var then m else Variable  v
beta var (Abs v m) n = beta v m n
beta var (Apply (Abs v1 m1) m2) n =
    beta var m1' n
    where
      m1' = beta v1 m1 m2




---------------------------------------------------------
-- Parser
variable :: Parser (Lambda)
variable =
    do { x <- many lower
       ;  return . Variable $ x
       }

abstract :: Parser (Lambda)
abstract =
    do { string "lambda" >> spaces
       ; (Variable x) <- variable
       ; spaces >> char '.'>> spaces
       ; m <- lambdaExpr
       ; return  (Abs x m)
       }

aplly :: Parser (Lambda )
aplly  =
    do { m <- lambdaExpr
       ; spaces
       ; n <- lambdaExpr
       ; return  (Apply  m n)
       }
parents :: Parser (Lambda) -> Parser (Lambda)
parents  p =
    do { char '('
       ; spaces
       ; x <- p
       ; spaces
       ; char ')'
       ; return x
       }

lambdaExpr :: Parser (Lambda )
lambdaExpr =
    do { (do {(parents abstract) <|>  variable <|>  aplly })
       }

{-
             <|>do { char '(' >> spaces
                   ; m <- lambdaExpr
                   ; spaces >> char ')'
                   ; return m
                   }
-}

---------------------------------------------------------
--
trans :: Lambda -> Maybe Lambda
trans (Apply (Abs v m) n) =
    return (beta v m n)
trans (Apply m n) =
    (do {m' <- trans m; return (Apply m' n)})
    `mplus`
    (do {n' <- trans n; return (Apply m n')})
trans (Abs v m) =
    do { m <- trans $ m
       ; return (Abs v m)
       }
trans _ = mzero

oper :: Lambda -> [Lambda]
oper m = m :  unfoldr (fmap fork . trans) m
    where
      fork x = (x,x)



prod  :: [[a]] -> [[a]]
prod = Data.List.foldr (\ xs ys -> [x:y | x <- xs , y <- ys]) [[]]



