{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use unless" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Homework8 where
import Data.Bifunctor (first)
import Control.Monad (void)
import Data.Char (isAlphaNum)
import System.IO
import System.Exit

data ComplicatedA a b
    = Con1 a b
    | Con2 [Maybe (a -> b)]

instance Functor (ComplicatedA a) where
    fmap :: (b -> c) -> ComplicatedA a b -> ComplicatedA a c
    fmap f (Con1 a b) = Con1 a (f b)
    fmap f (Con2 fs) = Con2 (fmap (fmap (f.)) fs)

showComplicated :: (Show a, Show b) => ComplicatedA a b -> String
showComplicated (Con1 a b) = "Con1 " <> show a <> " " <> show b
showComplicated (Con2 _) = "Con2"

applyComplicated :: ComplicatedA a b -> a -> [Maybe b]
applyComplicated (Con1 _ _) _ = []
applyComplicated (Con2 fs) value = fmap (fmap ($ value)) fs

func0 :: Monad f => (a -> a) -> f a -> f a
func0 f xs = do
    x <- xs
    return (f (f x))

func0' :: Functor f => (a -> a) -> f a -> f a
func0' f = fmap (f . f)

func1 :: Monad f => f a -> f (a,a)
func1 xs = xs >>= (\x -> return (x,x))

func1' :: Functor f => f a -> f (a, a)
func1' = fmap (\x -> (x, x))

-- we can't rewrite this one using functor
-- since we need the flatmap, as we apply flatmap twice while remaining in the same context
-- if you apply fmap with a function that also applies fmap, you'll end up with f (f b)
-- e.g. image f is the list type
-- then this function produces a list of lists, and flattens it into a single list
-- not possible to do with fmap
-- we can do it with applicative though
func2 :: Monad f => f a -> f (a,a)
func2 xs = xs >>= (\x -> xs >>= \y -> return (x,y))

func2' :: Applicative f => f a -> f (a, a)
func2' xs = (,) <$> xs <*> xs

-- for the same reasons as above this needs applicative
func3 :: Monad f => f a -> f (a,a)
func3 xs = xs >>= (\x -> xs >>= \y -> return (x,x))

func3' :: Applicative f => f a -> f (a, a)
func3' xs = (\x y -> (x, x)) <$> xs <*> xs

func4 :: Monad f => f a -> f a -> f (a,a)
func4 xs ys = xs >>= (\x -> ys >>= \y -> return (x,y))

func4' :: Applicative f => f a -> f a -> f (a, a)
func4' xs ys = (,) <$> xs <*> ys

func5 :: Monad f => f Integer -> f Integer -> f Integer
func5 xs ys = do
    x <- xs
    let x' = x + 1 -- same as x <- (+1) <$> xs
    y <- (+1) <$> ys
    return (x' + y)

func5' :: Applicative f => f Integer -> f Integer -> f Integer
func5' xs ys = (\x y -> (x+1) + (y+1)) <$> xs <*> ys

func6 :: Monad f => f Integer -> f (Integer,Integer)
func6 xs = do
    x <- xs
    return $ if x > 0 then (x, 0)
                      else (0, x)

func6' :: Functor f => f Integer -> f (Integer, Integer)
func6' = fmap (\x -> if x > 0 then (x, 0) else (0, x))

func7 :: Monad f => f Integer -> f (Integer,Integer)
func7 xs = do
    x <- xs
    if x > 0 then return (x, 0) -- return inside if is the same as return outside the if
             else return (0, x)

func7' :: Functor f => f Integer -> f (Integer, Integer)
func7' = fmap (\x -> if x > 0 then (x, 0) else (0, x))

func8 :: Monad f => f Integer -> Integer -> f Integer
func8 xs x = pure (+) <*> xs <*> pure x

func8' :: Functor f => f Integer -> Integer -> f Integer
func8' xs x = (+x) <$> xs

-- needs flatmap to return a Monad and "merge it into the outer monad context"
func9 :: Monad f => f Integer -> f Integer -> f Integer -> f Integer
func9 xs ys zs = xs >>= \x -> if even x then ys else zs

func10 :: Monad f => f Integer -> f Integer
func10 xs = do
    x <- xs >>= (\x -> return (x * x))
    return (x + 10)

func10' :: Functor f => f Integer -> f Integer
func10' = fmap (\x -> x*x + 10)

newtype Parser a = P (String -> Maybe (a, String))

runParser :: Parser a -> String -> Maybe (a, String)
runParser (P p) = p

parse :: Parser a -> String -> Maybe a
parse p input = case runParser p input of
    Just (result, "") -> Just result
    _ -> Nothing

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f parser = P $ fmap (Data.Bifunctor.first f) . runParser parser

instance Applicative Parser where
  pure :: a -> Parser a
  pure result = P (\input -> Just (result, input))

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) fp fx = P (\input -> do
    (parseFunc, rem1) <- runParser fp input
    (argument, rem2) <- runParser fx rem1
    Just (parseFunc argument, rem2))

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) fx fp = P (\input -> do
    (argument, rem1) <- runParser fx input
    runParser (fp argument) rem1)

noParser :: Parser a
noParser = P (const Nothing)

-- parse any character, if there is one
anyChar :: Parser Char
anyChar = P go
    where
        go :: String -> Maybe (Char, String)
        go "" = Nothing
        go (c:cs) = Just (c, cs)

-- parse any alpha numeric character
anyAlphaNum :: Parser Char
anyAlphaNum = anyChar >>= (\char' -> if Data.Char.isAlphaNum char' then pure char' else noParser)

-- try to find a character, if found then succeed (and return no input), otherwise fail
char :: Char -> Parser ()
char c = anyChar >>= (\char' -> if char' == c then pure () else noParser)

-- succeed if finding any character other than the one specified
anyCharBut :: Char -> Parser Char
anyCharBut c = anyChar >>= (\char' -> if char' /= c then pure char' else noParser)

-- try the left parser, if it fails then try the right parser
orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 = P (\str -> case runParser p1 str of
    result@(Just _) -> result
    Nothing -> runParser p2 str)

-- repeatedly apply a parser until it fails
many :: Parser a -> Parser [a]
many parser = orElse (parser >>= (\result -> (result:) <$> many parser)) (pure [])

-- apply a parser at least once, then apply until failure
oneOrMany :: Parser a -> Parser [a]
oneOrMany parser = parser >>= (\result -> (result:) <$> many parser)

-- run a parser zero or one times (greedily tries to run the parser once)
zeroOrOne :: Parser a -> Parser (Maybe a)
zeroOrOne parser = orElse (Just <$> parser) (pure Nothing)

-- repeatedly apply alternating parsers
sepBy :: Parser a -> Parser () -> Parser [a]
sepBy p1 p2 = (p1 >>= (\result -> (result:) <$> ((p2 *> sepBy p1 p2) `orElse` pure []))) `orElse` pure []

-- parse a string surrounded by the same char on both sides
enclosedBy :: Char -> Parser String
enclosedBy c = char c *> many (anyCharBut c) <* char c

-- parse a string surrounded by two different chars on each side
enclosed :: Char -> Char -> Parser String
enclosed leftChar rightChar = char leftChar *> many (anyCharBut rightChar) <* char rightChar

chainParser :: Parser String -> Parser a -> Parser a
chainParser p1 p2 = p1 >>= (\parsed -> maybe noParser pure (parse p2 parsed))

-- consume 0 or more spaces
spaces :: Parser ()
spaces = Control.Monad.void (many (char ' '))

parseCSV :: Parser [[String]]
parseCSV = many parseLine
  where
    parseLine = parseCell `sepBy` char ',' <* char '\n'
    parseCell = enclosedBy '"'

parseArray :: Parser [String]
parseArray = enclosed '[' ']' `chainParser` (many (anyCharBut ',') `sepBy` char ',')

-- INI File Parser
newtype Identifer = Identifier String deriving Show
newtype Value = Value String deriving Show
newtype Declaration = Declaration (Identifer, Value) deriving Show
newtype Section = Section (Identifer, [Declaration]) deriving Show
newtype INIFile = INIFile [Section] deriving Show

parseIdentifier :: Parser Identifer
parseIdentifier = Identifier <$> oneOrMany anyAlphaNum

parseValue :: Parser Value
parseValue = Value <$> many (anyCharBut '\n')

-- Parse an INI section header
parseHeader :: Parser Identifer
parseHeader = enclosed '[' ']' `chainParser` parseIdentifier

-- Parse a declaration of an INI variable
parseDeclaration :: Parser Declaration
parseDeclaration = do
    identifier <- parseIdentifier
    spaces
    char '='
    spaces
    value <- parseValue
    return $ Declaration (identifier, value)

-- Consume and ignore a comment
parseComment :: Parser ()
parseComment = char '#' <* many (anyCharBut '\n')

parseSection :: Parser Section
parseSection = do
    header <- parseHeader
    char '\n'
    newLinesAndComments
    declarations <- parseDeclaration `sepBy` newLinesAndComments
    return $ Section (header, declarations)
    where
        oneOrManyNewlines = Control.Monad.void (oneOrMany (char '\n'))
        newLinesAndComments = Control.Monad.void $ zeroOrOne parseComment `sepBy` oneOrManyNewlines

parseINI :: Parser INIFile
parseINI = INIFile <$> many parseSection

homeWork8Main :: IO ()
homeWork8Main = do
    -- contruct example of ComplicatedA
    let conExample = Con1 (1 :: Int) "Hello"
        in putStrLn $ showComplicated $ fmap (++ " World") conExample
    let conExample2 = Con2 [Just (+ (1:: Int)), Nothing]
        mapped = fmap ((++ " mapped") . show) conExample2
        in print $ applyComplicated mapped 1
    let parseExample = runParser (many (anyCharBut ')')) "(hello there) further text"
        in print parseExample
    let parseLines = runParser (many (anyCharBut '\n') `sepBy` char '\n') "hello\nworld\n!"
        in print parseLines
    let parsedCsv = parse parseCSV "\"ab\",\"cd\"\n\"\",\"de\"\n\n"
        in print parsedCsv
    let parsedArray = parse parseArray "[1,2,3,hello,4,5,9]"
        in print parsedArray

homeWork8INIMain :: IO ()
homeWork8INIMain = do
    input <- readFile "homework8.ini"
    case parse parseINI input of
        Just i -> print i
        Nothing -> do
            hPutStrLn stderr "Failed to parse INI file."
            exitFailure
