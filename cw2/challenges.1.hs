import Control.Applicative
import Data.Char
--challenge 1

data Expr = App Expr Expr | Lam Int Expr | Var Int deriving (Show, Eq)

--a
freeVariables :: Expr -> [Int]
freeVariables (App ex1 ex2) = freeVariables ex1 `union` freeVariables ex2
freeVariables (Lam i ex) = [x | x <- freeVariables ex, x /= i]
freeVariables (Var i) = [i]

union :: [Int] -> [Int] -> [Int]
union xs ys = [x | x <- xs, not (elem x ys)] ++ ys

--b
rename :: Expr -> Int -> Int -> Expr
rename (App ex1 ex2) old new = App (rename ex1 old new) (rename ex2 old new)
rename (Lam i ex) old new
    | i == old && isApplication ex && isLam first && isLam second = Lam new ex
    | i == old && isApplication ex && isLam first = Lam new (App first (rename second old new))
    | i == old && isApplication ex && isLam second = Lam new (App (rename first old new) second)
            | i == old && isLam ex && bound ex old = Lam new ex
            | i == old = Lam new (rename ex old new)
            | otherwise = Lam i (rename ex old new)
        where   sections = fromJust (getSections ex)
                first = fst sections
                second = snd sections
rename (Var i) old new
    | i == old = Var new
    | otherwise = Var i

fromJust :: (Maybe (Expr, Expr)) -> (Expr, Expr)
fromJust (Just e) = e

used :: Expr -> Int -> Bool
used (App ex1 ex2) i = or [used ex1 i, used ex2 i]
used (Lam j ex) i = or [i == j, used ex i]
used (Var j) i = i == j

bound :: Expr -> Int -> Bool
bound (App ex1 ex2) i = or [bound ex1 i, bound ex2 i]
bound (Lam j ex) i = or [i == j, bound ex i]
bound (Var j) i = False 

getSections :: Expr -> Maybe (Expr, Expr)
getSections (App ex1 ex2) = Just (ex1, ex2)
getSections _ = Nothing

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
equalExpr (App ex1 ex2) (App ex3 ex4) = or [equalExpr ex1 ex3 && equalExpr ex2 ex4, equalExpr ex1 ex4 && equalExpr ex2 ex3]
equalExpr (Lam i ex1) (Lam j ex2) = i == j && equalExpr ex1 ex2
equalExpr (Var i) (Var j) = i == j
equalExpr _ _ = False

alphaEquivalent :: Expr -> Expr -> Bool
alphaEquivalent (Var i) (Var j) = i == j
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
prettyPrint (App (Lam i ex1) (App ex2 ex3)) = "(" ++ prettyPrint (Lam i ex1) ++ ")(" ++ prettyPrint (App ex2 ex3) ++ ")"
prettyPrint (App (Lam i ex1) (Lam j ex2)) = "(" ++ prettyPrint (Lam i ex1) ++ ")(" ++ prettyPrint (Lam j ex2) ++ ")"
prettyPrint (App (Lam i ex1) ex2) = "(" ++ prettyPrint (Lam i ex1) ++ ")" ++ prettyPrint ex2
prettyPrint (App ex1 (App (Lam i ex2) ex3)) = prettyPrint ex1 ++ "(" ++ prettyPrint (App (Lam i ex2) ex3) ++ ")"
prettyPrint (App ex1 (App ex2 ex3)) = prettyPrint ex1 ++ "(" ++ prettyPrint (App ex2 ex3) ++ ")"
prettyPrint (App ex1 ex2) = prettyPrint ex1 ++ prettyPrint ex2
prettyPrint (Lam i (Lam j ex)) = "\\x" ++ show i ++ drop 1 (prettyPrint (Lam j ex))
prettyPrint (Lam i ex) = "\\x" ++ show i ++ "->" ++ prettyPrint ex
prettyPrint (Var i) = "x" ++ show i

anotherLam :: Expr -> Bool
anotherLam (Lam i ex) = True
anotherLam _ = False

isApplication :: Expr -> Bool
isApplication (App ex1 ex2) = True
isApplication _ = False

isLam :: Expr -> Bool
isLam (Lam i ex) = True
isLam _ = False

--challenge 3
data ExtExpr = ExtApp ExtExpr ExtExpr | ExtLam [Int] ExtExpr | ExtVar Int deriving (Show, Eq)
newtype Parser a = P (String -> [(a,String)])

parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
            [] -> []
            (x:xs) -> [(x,xs)])

instance Functor Parser where
    --fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\inp -> case parse p inp of
        [] -> []
        [(v,out)] -> [(g v, out)]) 

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> [(v,inp)])
    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of
        [] -> []
        [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
        [] -> []
        [(v,out)] -> parse (f v) out)

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\inp -> [])
    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
                [] -> parse q inp
                [(v,out)] -> [(v,out)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v


natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

parseLam :: String -> Maybe ExtExpr
parseLam xs
    | containsNothing (parseInterLam xs) = Nothing
    | otherwise = Just (convert (parseInterLam xs))

containsNothing :: Maybe InterExpr -> Bool
containsNothing Nothing = True
containsNothing (Just (InterApp ex1 ex2)) = or [containsNothing ex1, containsNothing ex2]
containsNothing (Just (InterLam xs ex)) = containsNothing ex
containsNothing (Just (InterVar i)) = False
containsNothing (Just (InterPlaceholder i)) = False

parseInterLam :: String -> Maybe InterExpr
parseInterLam xs = case (parse parseLamSection xs) of
                [(n,[])] -> n
                [(n,out)] -> connect n (parseInterLam out) 
                [] -> case (parse parseBracketSection xs) of
                    [(n, [])] -> n
                    [(n, out)] -> connect n (parseInterLam out)
                    [] -> case (parse parseVarsSection xs) of
                        [(Nothing, _)] -> Nothing
                        [(n, [])] -> n
                        [(n,out)] -> connect n (parseInterLam out)

data InterExpr = InterApp (Maybe InterExpr) (Maybe InterExpr) | InterLam [Int] (Maybe InterExpr) | InterVar Int | InterPlaceholder Int deriving Show

connect :: (Maybe InterExpr) -> (Maybe InterExpr) -> (Maybe InterExpr)
connect _ Nothing = Nothing
connect Nothing _ = Nothing
connect (Just (InterApp ex1 ex2)) e = Just (InterApp (connect ex2 e) (connect ex1 e))
connect (Just (InterLam xs ex)) e = Just (InterLam xs (connect ex e))
connect (Just (InterVar i)) e = Just (InterVar i)
connect (Just (InterPlaceholder i)) e = e

convert :: (Maybe InterExpr) -> ExtExpr
convert (Just (InterApp ex1 (Just (InterPlaceholder i)))) = convert ex1
convert (Just (InterApp ex1 ex2)) = ExtApp (convert ex2) (convert ex1)
convert (Just (InterLam xs ex)) = ExtLam xs (convert ex)
convert (Just (InterVar i)) = ExtVar i
convert (Just (InterPlaceholder i)) = ExtVar i

removePlaceholder :: (Maybe InterExpr) -> (Maybe InterExpr)
removePlaceholder (Just (InterApp (Just (InterPlaceholder i)) ex1)) = ex1
removePlaceholder (Just (InterApp ex1 (Just (InterPlaceholder i)))) = ex1
removePlaceholder (Just (InterApp ex1 ex2)) = Just (InterApp (removePlaceholder ex2) (removePlaceholder ex1))
--removePlaceholder (Just (InterApp ex1 ex2)) = Just (InterApp (removePlaceholder ex2) (removePlaceholder ex1))
removePlaceholder (Just (InterLam xs ex)) = Just (InterLam xs (removePlaceholder ex))
removePlaceholder (Just (InterVar i)) = (Just (InterVar i))


parseLamSection :: Parser (Maybe InterExpr)
parseLamSection = do symbol "\\"
                     ns <- many (do symbol "x"
                                    natural)
                     symbol "->"
                     return (Just (InterLam ns (Just (InterPlaceholder 1))))

parseBracketSection :: Parser (Maybe InterExpr)
parseBracketSection = do charBracket
                         ns <- some (do charNotBracket)
                         charBracket
                         return (Just (InterApp (removePlaceholder (parseInterLam ns)) (Just(InterPlaceholder 2))))

charNotBracket :: Parser Char
charNotBracket = sat notBracket

charBracket :: Parser Char
charBracket = sat bracket

bracket :: Char -> Bool
bracket x = or [x == '(', x == ')']

notBracket :: Char -> Bool
notBracket x = not (bracket x)
                                                 
parseVarsSection :: Parser (Maybe InterExpr)
parseVarsSection = do ns <- many (do symbol "x"
                                     natural)
                      return (constructVars (reverse ns))
               
constructVars :: [Int] -> Maybe InterExpr
constructVars [] = Nothing
constructVars (x:[]) = Just (InterApp (Just (InterVar x)) (Just (InterPlaceholder 1)))
constructVars (x:xs)
    | isNothing (constructVars xs) = Nothing
    | otherwise = Just (InterApp (Just (InterVar x)) (constructVars xs))

isNothing :: (Maybe InterExpr) -> Bool
isNothing (Nothing) = True
isNothing _ = False

--challenge 4
data ExprCL = AppCL ExprCL ExprCL | K | S | VarCL Int deriving (Show)

data ExprBoth = AppBoth ExprBoth ExprBoth | KBoth | SBoth | VarBoth Int | LamBoth Int ExprBoth deriving (Show)

translate :: Expr -> ExprCL
translate ex = fromBoth (translateInter (toBoth ex))

translateInter :: ExprBoth -> ExprBoth
--1
translateInter (VarBoth i) = VarBoth i
--2
translateInter (AppBoth ex1 ex2) = AppBoth (translateInter ex1) (translateInter ex2)
--3
translateInter (LamBoth i ex)
    | not (freeBoth ex i) = AppBoth KBoth (translateInter ex)
--4
translateInter (LamBoth i (VarBoth j))
    | i == j = AppBoth (AppBoth SBoth KBoth) KBoth
--5
translateInter (LamBoth i (LamBoth j ex)) = translateInter (LamBoth i (translateInter (LamBoth j ex)))
--6
translateInter (LamBoth i (AppBoth ex1 ex2)) = AppBoth (AppBoth SBoth (translateInter (LamBoth i ex1))) (translateInter (LamBoth i ex2))
--extras
translateInter (KBoth) = KBoth
translateInter (SBoth) = SBoth

toBoth :: Expr -> ExprBoth
toBoth (App ex1 ex2) = AppBoth (toBoth ex1) (toBoth ex2)
toBoth (Lam i ex) = LamBoth i (toBoth ex)
toBoth (Var i) = VarBoth i

fromBoth :: ExprBoth -> ExprCL
fromBoth (AppBoth ex1 ex2) = AppCL (fromBoth ex1) (fromBoth ex2)
fromBoth (KBoth) = K
fromBoth (SBoth) = S
fromBoth (VarBoth i) = VarCL i

freeBoth :: ExprBoth -> Int -> Bool
freeBoth (AppBoth ex1 ex2) i = or [freeBoth ex1 i, freeBoth ex2 i]
freeBoth (LamBoth j ex) i = i /= j && freeBoth ex i
freeBoth (VarBoth j) i = i == j
freeBoth _ i = False

--challenge 5
--proof in report