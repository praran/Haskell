
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
Imports
-----------------------------------------------------------------------------------------------------------------------------------------------------------------

> import Data.Char

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
Data Types
-----------------------------------------------------------------------------------------------------------------------------------------------------------------

> data Expr  = Const Double
>             | Var String
>             | Negate  
>             | Power Double
>             | Sqrt
>             | Opr Op
>             | Blank
>             | Equal
>  deriving (Show, Read)

> data Op = Plus | Minus | Mul | Divide deriving (Show,Read,Eq)

> data Tree = Emp 
>             | Node Tree Expr Tree
>  deriving (Show,Read)

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
Tokenizer
-----------------------------------------------------------------------------------------------------------------------------------------------------------------

> data Token = TokenOp Op
>             | TokenNum Double
>             | TokenVar String
>             | TokenPower
>             | TokenSqrt
>             | TokenLBracket
>             | TokenRBracket
>             | TokenEquals
>             | TokenEnd
>   deriving (Show,Read,Eq)

> tokenize :: String -> [Token]
> tokenize []  = []
> tokenize (x:xs)
>               | isSpace x       = tokenize xs
>               | x == '^'        = TokenPower      : tokenize xs
>               | x == '='        = TokenEquals     : tokenize xs
>               | x == '('        = TokenLBracket   : tokenize xs
>               | x == ')'        = TokenRBracket   : tokenize xs
>               | x == '+'        = TokenOp Plus    : tokenize xs
>               | x == '-'        = TokenOp Minus   : tokenize xs
>               | x == '*'        = TokenOp Mul     : tokenize xs
>               | x == '/'        = TokenOp Divide  : tokenize xs
>               | isDigit x       = asTokenNumber x xs
>               | isAlpha x       = asTokenVariable x xs
>               | otherwise       = tokenize xs

> asTokenNumber :: Char -> String -> [Token]
> asTokenNumber c cs = let (digit, cs') = span isDigit cs in
>                         TokenNum (read (c : digit)) : tokenize cs'

> asTokenVariable :: Char -> String -> [Token]
> asTokenVariable c cs
>                | x == "sqrt" = TokenSqrt : tokenize xs
>                | otherwise   = TokenVar x : tokenize xs
>                  where (x,xs) = span isAlphaNum $c:cs

> getHead :: [Token] -> Token
> getHead [] = TokenEnd
> getHead (x:xs) = x

> getTail :: [Token] -> [Token]
> getTail  []     =  error "nothing to get"
> getTail  (x:xs) = xs

[Reference : The general idea for tokenizer is taken from https://github.com/BartoszMilewski/SymCalc1/blob/master/src/Lexer.hs]

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
Parser
-----------------------------------------------------------------------------------------------------------------------------------------------------------------

> parse :: [Token] -> Tree
> parse  []     = Emp
> parse tokens 
>                 | elem TokenEquals tokens      = let (l,r) =  splitOnEquals tokens
>                                                    in Node (parse l) Equal (parse (tail r))
>                 | elem TokenPower tokens       = let (a, b, c) = splitOnPower tokens
>                                                    in  Node (parse a) (Power b) (parse c)
>                 | elem TokenLBracket tokens    = let (a,b) = extSubExpr tokens
>                                                    in Node (parse a) Blank (parse b)
>                 | containsOp tokens            = let (a,b,c) = splitOnOp tokens
>                                                    in Node (parse a) (Opr b) (parse c)
>                 | not . null $  tokens         = case getHead tokens   of
>                                                      (TokenNum d) -> Node (parse (tail tokens)) (Const d) Emp
>                                                      (TokenVar s) -> Node Emp (Var s) (parse (tail tokens))
>                                                      _            -> Emp

> splitOnEquals :: [Token] -> ([Token],[Token])
> splitOnEquals  tokens    = let (a, b) = break (== TokenEquals) tokens
>                              in (a, getTail b)
>                                

> splitOnPower :: [Token] -> ([Token],Double,[Token])
> splitOnPower tokens = let (c,d) = break (== TokenPower) tokens
>                         in 
>                           case getHead(getTail d) of
>                             (TokenNum v) -> (c, v, getTail(getTail d))
>                             _            -> error "Power of variable missing"

> extSubExpr :: [Token] -> ([Token],[Token])
> extSubExpr  tokens = let (c,d) = break (== TokenRBracket) tokens
>                         in 
>                           case getHead c of
>                                (TokenLBracket) -> (tail c , tail d)
>                                _               -> (c,d)

> splitOnOp :: [Token] -> ([Token],Op,[Token])
> splitOnOp  tokens    = (x, extTokenOp (head xs) , tail xs)
>                          where (x,xs) = break isTokenOp tokens

> extTokenOp :: Token -> Op
> extTokenOp  token = case token of
>                       (TokenOp op) -> op
>                       _            -> error "illegal operator"

> containsOp :: [Token] -> Bool
> containsOp []      = False
> containsOp (x:xs)  
>                  | isTokenOp x = True
>                  | otherwise   = containsOp xs

> isTokenOp :: Token -> Bool
> isTokenOp  token = case token of
>                       (TokenOp op) -> True
>                       _            -> False

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
Formula to find roots  quadratic equations-----------------------------------------------------------------------------------------------------------------------------------------------------------------

General Formula for : ax^2 + bx +c =0

                  x = -b/2a + sqrt (b^2-4*a*c) , -b/2a - sqrt (b^2-4*a*c)


				   
> roots :: (Double ,Double , Double) ->(Double,Double)				   
> roots (a,b,c) = if d < 0 then error "0" else (x, y)
>                        where
>                          x = e + sqrt d / (2 * a)
>                          y = e - sqrt d / (2 * a)
>                          d = b * b - 4 * a * c
>                          e = - b / (2 * a)				   


-----------------------------------------------------------------------------------------------------------------------------------------------------------------
Evaluator
-----------------------------------------------------------------------------------------------------------------------------------------------------------------

> evaluate :: Tree -> (Double,Double,Double)
> evaluate = undefined

> result :: String -> (Double,Double)
> result s = roots $ evaluate $ parse $ tokenize s


> equationSolver :: IO()
> equationSolver =
>                putStr "Please enter the equation to solve. \n Usage enter square of x as x^2 \n" >>
>                getLine >>= \s -> putStr ("Entered equation is  " ++ s ++ "\n" ++ "The Roots are" ++ show (result s))
