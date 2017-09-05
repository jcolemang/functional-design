
module CompanyParsers where

import Types

import Control.Monad
import Text.ParserCombinators.Parsec


googleParser :: Types.Parser
googleParser s =
  let p = do
        k <- string "geo"
        num <- many1 digit
        void $ space >> string "-" >> space
        v <- many1 digit
        return [(k++num, v)]
  in case parse p "" s of
    Right val ->
      Right val
    Left _ ->
      Left BadParse

google :: CompanyParser
google = CParser "google" googleParser

microsoftParser :: Types.Parser
microsoftParser s =
  let p = do
        k <- string "ms"
        num <- many1 digit
        void $ string ","
        v <- many1 digit
        return [(k++num, v)]
  in case parse p "" s of
    Right val ->
      Right val
    Left _ ->
      Left BadParse

microsoft :: CompanyParser
microsoft = CParser "microsoft" microsoftParser

amazonParser :: Types.Parser
amazonParser s =
  let p = do
        k1 <- string "aws"
        num1 <- many1 digit
        void $ space >> string "ttl" >> space
        v1 <- many1 digit
        void $ string "," >> space
        k2 <- string "aws"
        num2 <- many1 digit
        void $ space >> string "ttl" >> space
        v2 <- many1 digit
        return [(k1 ++ num1, v1), (k2 ++ num2, v2)]
  in case parse p "" s of
    Right val ->
      Right val
    Left _ ->
      Left BadParse

amazon :: CompanyParser
amazon = CParser "amazon" amazonParser

defaultParsers :: [CompanyParser]
defaultParsers =
  [ google
  , microsoft
  , amazon
  ]
