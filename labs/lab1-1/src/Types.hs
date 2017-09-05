
module Types where

data ParseError
  = NoParser String
  | EmptyInput
  | BadParse
  deriving (Show)

type Parser = String -> Either ParseError [(String, String)]

data CompanyParser
  = CParser String Parser
