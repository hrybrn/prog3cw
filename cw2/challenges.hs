import Data.List
import Data.Char
import Data.List.Split
import Data.Maybe
--challenge 1

data Expr = App Expr Expr | Lam Int Expr | Var Int deriving (Show, Eq)

--a
freeVariables :: Expr -> [Int]
freeVariables (App ex1 ex2) = freeVariables ex1 `union` freeVariables ex2
freeVariables (Lam i ex) = freeVariables ex \\ [i]
freeVariables (Var i) = [i]

union :: [Int] -> [Int] -> [Int]
union [] ys = 
union (x:xs) ys
    | elem x ys = union xs ys
    | otherwise = [x] ++ union xs ys

--b
rename :: Expr -> Int -> Int -> Expr
rename (App ex1 ex2) old new = App (rename ex1 old new) (rename ex2 old new)
rename (Lam i ex) old new
    | i == old && elem i (freeVariables ex) = Lam new (rename ex old new)
    | otherwise = Lam i (rename ex old new)
rename (Var i) old new
    | i == old = Var new
    | otherwise = Var i

used :: Expr -> Int -> Bool
used (App ex1 ex2) i = or [used ex1 i, used ex2 i]
used (Lam j ex) i = or [i == j, used ex i]
used (Var j) i = i == j

--c
--this shows what variables have been defined and used
variablesUsed :: Expr -> [Int]
variablesUsed (App ex1 ex2) = variablesUsed ex1 ++ variablesUsed ex2
variablesUsed (Lam i ex) = variablesUsed ex ++ [i]
variablesUsed (Var i) = [i]

--two Exprs have the same types at each level
typeEquivalence :: Expr -> Expr -> Bool
typeEquivalence (App ex1 ex2) (App ex3 ex4) = typeEquivalence ex1 ex3 && typeEquivalence ex2 ex4
typeEquivalence (Lam i ex1) (Lam j ex2) = typeEquivalence ex1 ex2
typeEquivalence (Var i) (Var j) = True
typeEquivalence _ _ = False

--check for equivalence
equalExpr :: Expr -> Expr -> Bool
equalExpr (App ex1 ex2) (App ex3 ex4) = equalExpr ex1 ex3 && equalExpr ex2 ex4
equalExpr (Lam i ex1) (Lam j ex2) = i == j && equalExpr ex1 ex2
equalExpr (Var i) (Var j) = i == j
equalExpr _ _ = False

alphaEquivalent :: Expr -> Expr -> Bool
alphaEquivalent ex1 ex2
    | equalExpr ex1 ex2 = True
    | typeEquivalence ex1 ex2 = canBeConverted ex1 ex2
    | otherwise = False

canBeConverted :: Expr -> Expr -> Bool
canBeConverted ex1 ex2 = length [x | x <- poss, equalExpr (rename ex1 (fst x) (snd x)) ex2] > 0
        where poss = possible ex1 ex2

possible :: Expr -> Expr -> [(Int, Int)]
possible (App ex1 ex2) (App ex3 ex4) = possible ex1 ex3 ++ possible ex2 ex4
possible (Lam i ex1) (Lam j ex2)
    | i == j = possible ex1 ex2
    | otherwise = [(i,j)] ++ possible ex1 ex2
possible (Var i) (Var j)
    | i == j = []
    | otherwise = [(i,j)]
possible _ _ = []

--d
hasRedex :: Expr -> Bool
hasRedex ex = length (freeVariables ex) > 0

--e
substitute :: Expr -> Int -> Expr -> Expr
substitute (App ex1 ex2) old new = App (substitute ex1 old new) (substitute ex2 old new)
substitute (Lam i ex) old new
    | i == old = Lam i ex
    | otherwise = Lam i (substitute ex old new)
substitute (Var i) old new
    | i == old = new
    | otherwise = Var i

--challenge 2
prettyPrint :: Expr -> String
prettyPrint (App ex1 ex2) = prettyPrint ex1 ++ prettyPrint ex2
prettyPrint (Lam i ex)
    | anotherLam ex = "\\x" ++ show i ++ prettyPrint ex
    | otherwise = "\\x" ++ show i ++ "->" ++ prettyPrint ex
prettyPrint (Var i) = "x" ++ show i

anotherLam :: Expr -> Bool
anotherLam (Lam i ex) = True
anotherLam (App ex1 ex2) = anotherLam ex1
anotherLam (Var i) = False

--challenge 3
data ExtExpr = ExtApp ExtExpr ExtExpr | ExtLam [Int] ExtExpr | ExtVar Int deriving (Show, Eq)
parseLam :: String -> Maybe ExtExpr
parseLam (x:xs)
    | isNothing vars = Nothing
    | x == '\\' && isNothing (afterArrow xs) = Nothing
    | x == '\\' = Just (ExtLam (fromJust vars) (fromJust (parseLam (fromJust (afterArrow xs)))))
    | x == 'x' = Just (apply (fromJust vars))
        where vars = makeVars (x:xs)
    
--parseLam :: String -> ExtExpr
--parseLam (x:xs)
--    | x == '\\' = ExtLam (makeVars xs) (parseLam (afterArrow xs))
--    | x == 'x' = apply (makeVars (x:xs))

makeVars :: String -> Maybe [Int]
makeVars xs
    | head xs == '\\' = makeVars (tail xs)
    | [stringToInt x | x <- split, x /= ""] == [] = Nothing
    | otherwise = Just [stringToInt x | x <- split, x /= ""]
        where split = splitOn "x" (head (splitOn " " (head (splitOn "\\" xs))))

afterArrow :: String -> Maybe String
afterArrow xs
    | take 4 xs == " -> " = Just (drop 4 xs)
    | isInfixOf " -> " xs == False = Nothing
    | otherwise = afterArrow (drop 1 xs)

apply :: [Int] -> ExtExpr
apply (x:[]) = ExtVar x
apply (x:xs) = ExtApp (ExtVar x) (apply xs)

stringToInt :: String -> Int
stringToInt (x:xs) = digitToInt x * 10 ^ length xs + stringToInt xs
stringToInt [] = 0

--challenge 4
data ExprCL = AppCL ExprCL ExprCL | K | S | VarCL Int deriving (Show)

k :: ExprCL -> ExprCL -> ExprCL
k x1 x2 = x2

s :: ExprCL -> ExprCL -> ExprCL -> ExprCL
s x1 x2 x3 = AppCL (AppCL x1 x2) (AppCL x1 x3)



translate :: Expr -> ExprCL
translate (App ex1 ex2) = AppCL (translate ex1) (translate ex2)
translate (Lam i ex)
    | isApplication ex && elem i (freeVariables ex) = AppCL S (translate (App (Lam i (fst (fromJust (getSections ex)))) (Lam i (snd (fromJust (getSections ex))))))
    | ex == Var i = AppCL (AppCL S K) K
    | elem i (freeVariables ex) == False = AppCL K (translate ex)
    | otherwise = translateInter i (translate ex)
translate (Var i) = VarCL i

translateInter :: Int -> ExprCL -> ExprCL
translateInter i ex 

isApplication :: Expr -> Bool
isApplication (App ex1 ex2) = True
isApplication _ = False

getSections :: Expr -> Maybe (Expr, Expr)
getSections (App ex1 ex2) = Just (ex1, ex2)
getSections _ = Nothing
--challenge 5