import Imp
import Text.Parsec (alphaNum, eof, letter, parseTest, (<|>))
import Text.Parsec.Expr
  ( Assoc (..),
    Operator (..),
    buildExpressionParser,
  )
import Text.Parsec.String (Parser, parseFromFile)
import qualified Text.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language
  ( GenLanguageDef (..),
    LanguageDef,
    emptyDef,
  )

impLanguageDef :: LanguageDef ()
impLanguageDef =
  emptyDef
    { commentStart = "/*",
      commentEnd = "*/",
      commentLine = "//",
      nestedComments = False,
      caseSensitive = True,
      identStart = letter,
      identLetter = alphaNum,
      reservedNames =
        [ "while",
          "if",
          "else",
          "int",
          "bool",
          "true",
          "false",
          "read",
          "print"
        ],
      reservedOpNames =
        [ "+",
          "-",
          "*",
          "/",
          "%",
          "==",
          "!=",
          "<",
          "<=",
          ">=",
          ">",
          "&&",
          "||",
          "!",
          "="
        ]
    }

impLexer :: Token.TokenParser ()
impLexer = Token.makeTokenParser impLanguageDef

identifier :: Parser String
identifier = Token.identifier impLexer

reserved :: String -> Parser ()
reserved = Token.reserved impLexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp impLexer

parens :: Parser a -> Parser a
parens = Token.parens impLexer

braces :: Parser a -> Parser a
braces = Token.braces impLexer

semiSep :: Parser a -> Parser [a]
semiSep = Token.semiSep impLexer

integer :: Parser Integer
integer = Token.integer impLexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace impLexer

string :: Parser String
string = Token.stringLiteral impLexer

comma :: Parser String
comma = Token.comma impLexer

ifStmt :: Parser Stmt
ifStmt = do
  reserved "if"
  cond <- parens expression
  thenS <- statement
  reserved "else"
  elseS <- statement
  return (If cond thenS elseS)

statement :: Parser Stmt
statement =
  assignment
    <|> ifStmt
    <|> readStmt
    <|> printStmt

assignment :: Parser Stmt
assignment = do
  id <- identifier
  reservedOp "="
  val <- expression
  return $ Asgn id val

-- read("n=", n);
readStmt :: Parser Stmt
readStmt = do
  reserved "read"
  parens parseRead

parseRead :: Parser Stmt
parseRead = do
  str <- string
  comma
  var <- identifier
  return (Read str var)

-- print("Is prime: ", n)
printStmt :: Parser Stmt
printStmt = do
  reserved "print"
  parens parsePrint

parsePrint :: Parser Stmt
parsePrint = do
  str <- string
  comma
  expr <- expression
  return (Print str expr)

expression :: Parser Exp
expression = buildExpressionParser operators term
  where
    operators =
      [ [ prefix "!" Not
        ],
        [ binary "*" (BinA Mul) AssocLeft
        ],
        [ binary "+" (BinA Add) AssocLeft
        ],
        [ binary "==" (BinE Eq) AssocNone,
          binary "<=" (BinC Lte) AssocNone
        ],
        [ binary "&&" (BinL And) AssocLeft,
          binary "||" (BinL Or) AssocLeft
        ]
      ]
    binary name fun = Infix (reservedOp name >> return fun)
    prefix name fun = Prefix (reservedOp name >> return fun)

term :: Parser Exp
term =
  parens expression
    <|> (I <$> integer)
    <|> (Id <$> identifier)

test = parseTest ifStmt "if ( 1 == 0 ) i = 1 else i = 0"

test1 = parseTest readStmt "read(\"n=\", n);"

test2 = parseTest printStmt "print(\"Is prime: \", n)"