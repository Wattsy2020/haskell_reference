{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use unless" #-}
module Homework9 where

import Data.Char
import Data.List

import System.Environment
import System.IO
import System.Exit
import Control.Monad (void)

-- Exercise 1

parseBNF :: Descr f => f BNF
parseBNF = pure []

-- Example: Simple expressions:

data Expr = Plus Expr Expr | Mult Expr Expr | Factorial Expr | Const Integer
    deriving Show

mkPlus :: Expr -> [Expr] -> Expr
mkPlus = foldl Plus

mkMult :: Expr -> [Expr] -> Expr
mkMult = foldl Mult

parseExpr :: Descr f => f Expr
parseExpr = recNonTerminal "expr" ePlus

ePlus :: Descr f => f Expr -> f Expr
ePlus expr = nonTerminal "plus" $
    mkPlus <$> eMult expr
           <*> many (spaces *>  char '+' *>  spaces *> eModifiedAtom expr)
           <*  spaces

eMult :: Descr f => f Expr -> f Expr
eMult expr = nonTerminal "mult" $
    mkPlus <$> eAtom expr
           <*> many (spaces *>  char '*' *>  spaces *> eModifiedAtom expr)
           <*  spaces

eModifiedAtom :: Descr f => f Expr -> f Expr
eModifiedAtom expr = nonTerminal "modified atom" $
    eFactorial expr `orElse` eAtom expr

eFactorial :: Descr f => f Expr -> f Expr
eFactorial expr = nonTerminal "factorial" $
    Factorial <$> (eAtom expr <* char '!')

eAtom :: Descr f => f Expr -> f Expr
eAtom expr = nonTerminal "atom" $
    aConst `orElse` eParens expr

aConst :: Descr f => f Expr
aConst = nonTerminal "const" $ Const . read <$> many1 digit

eParens :: Descr f => f a -> f a
eParens inner =
    id <$  char '('
       <*  spaces
       <*> inner
       <*  spaces
       <*  char ')'
       <*  spaces


-- simpler example grammar / parser
data SimpleExpr = ConstS Int | FactorialS SimpleExpr deriving Show

simpleExpr :: Descr f => f SimpleExpr
simpleExpr = recNonTerminal "expr" $ \expr ->
    simpleFactorial expr `orElse` simpleConst

simpleFactorial :: Descr f => f SimpleExpr -> f SimpleExpr
simpleFactorial expr = nonTerminal "factorial" $
    FactorialS <$> (char '!' *> expr)
-- funnily enough I don't think it's possible to have '!' at the end here
-- since recursion will try to check if the expr can parse first, it won't look for a '!' at the end yet

simpleConst :: Descr f => f SimpleExpr
simpleConst = nonTerminal "const" $
    ConstS . read <$> many1 digit

-- Start on BNF parser

descriptorBNF :: Descr f => f BNF
descriptorBNF = undefined

identifierBNF :: Descr f => f BNF
identifierBNF = nonTerminal "identifier" $
    (\str -> [(str, Terminal "")]) -- todo: Terminal "" needs to have the actual RHS the identifier is assigned to
    <$> 
    ((:) <$> letter <*> many (letter `orElse` digit `orElse` literalChar '-'))


-- EBNF in Haskell

data RHS
  = Terminal String
  | NonTerminal String
  | Choice RHS RHS
  | Sequence RHS RHS
  | Optional RHS
  | Repetition RHS
  deriving (Show, Eq)

mkChoices :: RHS -> [RHS] -> RHS
mkChoices = foldl Choice

mkSequences :: RHS -> [RHS] -> RHS
mkSequences = foldl Sequence

ppRHS :: RHS -> String
ppRHS = go (0 :: Integer)
  where
    go _ (Terminal s)     = surround "'" "'" $ concatMap quote s
    go _ (NonTerminal s)  = s
    go a (Choice x1 x2)   = p a 1 $ go 1 x1 ++ " | " ++ go 1 x2
    go a (Sequence x1 x2) = p a 2 $ go 2 x1 ++ ", "  ++ go 2 x2
    go _ (Optional x)     = surround "[" "]" $ go 0 x
    go _ (Repetition x)   = surround "{" "}" $ go 0 x

    surround c1 c2 x = c1 ++ x ++ c2

    p a n | a > n     = surround "(" ")"
          | otherwise = id

    quote '\'' = "\\'"
    quote '\\' = "\\\\"
    quote c    = [c]

type Production = (String, RHS)
type BNF = [Production]

ppBNF :: BNF -> String
ppBNF = unlines . map (\(i,rhs) -> i ++ " = " ++ ppRHS rhs ++ ";")

-- The parser

newtype Parser a = P (String -> Maybe (a, String))

runParser :: Parser t -> String -> Maybe (t, String)
runParser (P p) = p

parse :: Parser a -> String -> Maybe a
parse p input = case runParser p input of
    Just (result, "") -> Just result
    _ -> Nothing -- handles both no result and leftover input

noParserP :: Parser a
noParserP = P (const Nothing)

pureParserP :: a -> Parser a
pureParserP x = P (\input -> Just (x,input))

instance Functor Parser where
    fmap f p = P p'
      where
        p' input = case runParser p input of
            Just (result, rest) -> Just (f result, rest)
            Nothing             -> Nothing

instance Applicative Parser where
    pure = pureParserP
    p1 <*> p2 = P $ \input -> do
        (f, rest1) <- runParser p1 input
        (x, rest2) <- runParser p2 rest1
        return (f x, rest2)

instance Monad Parser where
    return = pure
    p1 >>= k = P $ \input -> do
        (x, rest1) <- runParser p1 input
        runParser (k x) rest1

anyCharP :: Parser Char
anyCharP = P $ \input -> case input of
    (c:rest) -> Just (c, rest)
    []       -> Nothing

charP :: Char -> Parser ()
charP c = do
    c' <- anyCharP
    if c == c' then return ()
               else noParserP

anyCharButP :: Char -> Parser Char
anyCharButP c = do
    c' <- anyCharP
    if c /= c' then return c'
               else noParserP

letterOrDigitP :: Parser Char
letterOrDigitP = do
    c <- anyCharP
    if isAlphaNum c then return c else noParserP

orElseP :: Parser a -> Parser a -> Parser a
orElseP p1 p2 = P $ \input -> case runParser p1 input of
    Just r -> Just r
    Nothing -> runParser p2 input

manyP :: Parser a -> Parser [a]
manyP p = ((:) <$> p <*> manyP p) `orElseP` return []

many1P :: Parser a -> Parser [a]
many1P p = ((:) <$> p) <*> manyP p

sepByP :: Parser a -> Parser () -> Parser [a]
sepByP p1 p2 = ((:) <$> p1 <*> manyP (p2 >> p1)) `orElseP` return []

-- A grammar-producing type constructor

newtype Grammar a = G (BNF, RHS)

runGrammer :: String -> Grammar a -> BNF
runGrammer main (G (prods, NonTerminal nt)) | main == nt = prods
runGrammer main (G (prods, rhs)) = prods ++ [(main, rhs)]

ppGrammar :: String -> Grammar a -> String
ppGrammar main g = ppBNF $ runGrammer main g

charG :: Char -> Grammar ()
charG c = G ([], Terminal [c])

anyCharG :: Grammar Char
anyCharG = G ([], NonTerminal "char")

manyG :: Grammar a -> Grammar [a]
manyG (G (prods, rhs)) = G (prods, Repetition rhs)

mergeProds :: [Production] -> [Production] -> [Production]
mergeProds prods1 prods2 = nub $ prods1 ++ prods2

orElseG :: Grammar a -> Grammar a -> Grammar a
orElseG (G (prods1, rhs1)) (G (prods2, rhs2))
    = G (mergeProds prods1 prods2, Choice rhs1 rhs2)

instance Functor Grammar where
    fmap _ (G bnf) = G bnf

instance Applicative Grammar where
    pure _ = G ([], Terminal "")
    G (prods1, Terminal "") <*> G (prods2, rhs2) = G (mergeProds prods1 prods2, rhs2)
    G (prods1, rhs1) <*> G (prods2, Terminal "") = G (mergeProds prods1 prods2, rhs1)
    G (prods1, rhs1) <*> G (prods2, rhs2) = G (mergeProds prods1 prods2, Sequence rhs1 rhs2)

many1G :: Grammar a -> Grammar [a]
many1G p = ((:) <$> p) <*> manyG p

sepByG :: Grammar a -> Grammar () -> Grammar [a]
sepByG p1 p2 = ((:) <$> p1 <*> manyG (p2 *> p1)) `orElseG` pure []

primitiveG :: String -> Grammar a
primitiveG s = G ([], NonTerminal s)

newlineG :: Grammar ()
newlineG = primitiveG "newline"

recNonTerminalG :: String -> (Grammar a -> Grammar a) -> Grammar a
recNonTerminalG name f =
    let G (prods, rhs) = f (G ([], NonTerminal name))
    in G (prods ++ [(name, rhs)], NonTerminal name)



-- The generic approach

class Applicative f => Descr f where
    char :: Char -> f ()
    many :: f a -> f [a]
    orElse :: f a -> f a -> f a
    primitive :: String -> Parser a -> f a
    recNonTerminal :: String -> (f a -> f a) -> f a

instance Descr Parser where
    char :: Char -> Parser ()
    char = charP

    many :: Parser a -> Parser [a]
    many = manyP

    orElse :: Parser a -> Parser a -> Parser a
    orElse = orElseP

    primitive :: String -> Parser a -> Parser a
    primitive _ p = p

    recNonTerminal :: String -> (Parser a -> Parser a) -> Parser a
    recNonTerminal _ p = let r = p r in r

instance Descr Grammar where
    char :: Char -> Grammar ()
    char = charG

    many :: Grammar a -> Grammar [a]
    many = manyG

    orElse :: Grammar a -> Grammar a -> Grammar a
    orElse = orElseG

    primitive :: String -> Parser a -> Grammar a
    primitive s _ = primitiveG s

    recNonTerminal :: String -> (Grammar a -> Grammar a) -> Grammar a
    recNonTerminal = recNonTerminalG

many1 :: Descr f => f a -> f [a]
many1 p = ((:) <$> p) <*> many p

sepBy :: Descr f => f a -> f () -> f [a]
sepBy p1 p2 = ((:) <$> p1 <*> many (p2 *> p1)) `orElse` pure []

nonTerminal :: Descr f => String -> f a -> f a
nonTerminal name p = recNonTerminal name (const p)

anyChar :: Descr f => f Char
anyChar = primitive "char" anyCharP

literalChar :: Descr f => Char -> f Char
literalChar c = c <$ char c

charChoice :: Descr f => [Char] -> f Char
charChoice = foldl1 orElse . map literalChar

newline :: Descr f => f ()
newline = nonTerminal "newline" $ char '\n'

letter :: Descr f => f Char
letter = primitive "letter" $ do
    c <- anyCharP
    if isLetter c then return c else noParserP

digit :: Descr f => f Char
digit = nonTerminal "digit" $ charChoice "0123456789"

notQuoteOrBackslash :: Descr f => f Char
notQuoteOrBackslash = primitive "non-quote-or-backslash" $ do
    c <- anyCharP
    if c `notElem` "\\'" then return c else noParserP

spaces :: Descr f => f ()
spaces = nonTerminal "spaces" $ Control.Monad.void $ many (char ' ' `orElse` newline)

homeWork9Main :: IO ()
homeWork9Main = do
    args <- getArgs
    case args of
        [] -> do
            putStr $ ppGrammar "bnf" parseBNF
        [fileName] -> do
            input <- readFile fileName
            case parse parseBNF input of
                Just i -> putStr $ ppBNF i
                Nothing -> do
                    hPutStrLn stderr "Failed to parse BNF file."
                    exitFailure
        _ -> hPutStrLn stderr "Too many arguments given" >> exitFailure
