
module ParseMicroc
    ( run
    ) where


import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P


endStr = "\n"
startStrExcludeLabel = "\t"
startStrLabel = ""


concatenate :: [String] -> String
concatenate ls = foldr (++) "" ls


run :: String -> String
run str = either show id $ parse input "microc(c->asm)" str

lexer :: P.TokenParser ()
lexer = P.makeTokenParser haskellDef --(emptyDef { reservedOpNames = ["&","&&"] })
reservedOp  = P.reservedOp lexer
integer     = P.integer lexer

number :: Parser Integer
number = integer <?> "integer"

name :: Parser String
name = do
    x <- letter
    xs <- many alphaNum
    return $ x:xs

factor :: Parser String
factor = try( do 
            spaces
            char '('
            spaces
            x <- expr
            spaces
            char ')'
            spaces
            return x
            )
        <|> do
            spaces
            x <- number
            spaces
            return $ startStrExcludeLabel++"PUSHI "++(show x)++endStr
        <|> try (do
            spaces
            string "in"
            notFollowedBy alphaNum
            spaces
            return  $ startStrExcludeLabel++"IN"++endStr
            )    
        <|> do
            spaces
            s <- name
            spaces
            return $ startStrExcludeLabel++"PUSH "++s++endStr -- OUTPUT
        <?> "simple expression"    



expr :: Parser String
expr = buildExpressionParser table factor
      <?> "expression"

table = [
        [unary "-" "NEG",unary "!" "NOT",unary "~" "BNOT"]
        ,[biop "*" "MUL" AssocLeft]
        ,[biop "+" "ADD" AssocLeft, biop "-" "SUB" AssocLeft]
        ,[biop "<<" "SHL" AssocLeft, biop ">>" "SHR" AssocLeft]
        ,[biop ">=" "GE" AssocLeft, biop "<=" "L " AssocLeft,biop ">" "GT" AssocLeft, biop "<" "LT" AssocLeft]
        ,[biop "!=" "NE" AssocLeft]
        ,[biop "==" "EQ" AssocLeft]
        ,[biop' '&' "BAND" AssocLeft]
        ,[biop "^" "BXOR" AssocLeft]
        ,[biop' '|' "BOR" AssocLeft]
        ,[biop "&&" "AND" AssocLeft]
        ,[biop "||" "OR" AssocLeft]
        ]
      where
        biop s f assoc = Infix ( try ( do{ string s;return $ fbiop f }) ) assoc
        fbiop f a b = a++b++startStrExcludeLabel++f++endStr
        
        biop' c f assoc = Infix ( try (  do{ char c; notFollowedBy (char c) ; return $ fbiop' f } ) ) assoc
        fbiop' f a b = a++b++startStrExcludeLabel++f++endStr

        unary s f = Prefix (do{string s ; return $funop f})
        funop f b = b++startStrExcludeLabel++f++endStr
assign::Parser String
assign = do
        spaces
        s <- name
        spaces
        char '='
        spaces
        e <- expr
        spaces
        char ';'
        spaces
        return $ e++startStrExcludeLabel++"POP "++s++endStr -- OUTPUT

out::Parser String
out = do
        spaces
        string "out"
        spaces
        char '('
        spaces
        s <- expr
        spaces
        char ')'
        spaces
        char ';'
        spaces
        return $ s++startStrExcludeLabel++"OUT"++endStr -- OUTPUT
        <?> "out"

halt::Parser String
halt = do
        spaces
        string "halt"
        spaces
        char ';'
        spaces
        return $ startStrExcludeLabel++"HALT"++endStr   -- OUTPUT  

goto::Parser String
goto = do
        spaces
        string "goto"
        spaces
        s <- name
        spaces
        char ';'
        spaces
        return $ startStrExcludeLabel++"JMP "++s++endStr -- OUTPUT

iF::Parser String
iF = do
        spaces
        string "if"
        spaces
        char '('
        spaces
        s<-expr
        spaces
        char ')'
        spaces
        string "goto"
        spaces
        n <- name
        spaces
        char ';'
        spaces
        return $ s++startStrExcludeLabel++"JNZ "++n++endStr -- OUTPUT
        <?> "if"

uNLESS::Parser String
uNLESS = do
        spaces
        string "unless"
        spaces
        char '('
        spaces
        s<-expr
        spaces
        char ')'
        spaces
        string "goto"
        spaces
        n <- name
        spaces
        char ';'
        spaces
        return $ startStrExcludeLabel++s++"JZ "++n++endStr -- OUTPUT
        <?> "uNLESS"
        
iNTEGER::Parser String
iNTEGER = try ( do
        spaces
        s <- name
        spaces
        char '='
        spaces
        x <- number
        spaces
        return $ startStrLabel++s++": "++(show x)
        )
    <|> do
        spaces
        s <- name
        spaces
        return $ startStrLabel++s++": 0"++endStr -- " ; " -- OUTPUT
    <?> "iNTEGER"    

intlist::Parser String
intlist = do
        spaces
        x <- iNTEGER
        xs <- many ( do
            spaces
            char ','
            spaces
            y <- iNTEGER
            spaces
            return y 
            )
        return $ concatenate $ x:xs
        <?> "intlist"

intdef::Parser String
intdef = do
        spaces
        string "int"
        spaces
        s <- intlist
        spaces
        char ';'
        spaces
        return s
        <?> "intdef"

lABLE::Parser String
lABLE = do
        spaces
        s <- name
        spaces
        char ':'
        spaces
        return $ startStrLabel++s ++":"++endStr -- OUTPUT
        <?> "lABEL"

statement::Parser String
statement = try lABLE <|> try intdef <|> try goto <|> try iF <|> try uNLESS <|> try halt <|> try out <|> try assign <?> "statement"

input::Parser String
input =  do 
        xs <- many1 statement 
        return $ concatenate xs
        <?> "input"

