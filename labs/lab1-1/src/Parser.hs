
module Parser where


import Types

import Data.Map
import Control.Monad


toCompanyX :: (String, String) -> String
toCompanyX (l, r) =
  l ++ " : " ++ r

nameAndLines :: String
             -> Either ParseError (String, [String])
nameAndLines content =
  let ls = lines content
  in case ls of
    [] -> Left EmptyInput
    name:rest -> return (name, rest)

parse :: [CompanyParser]
      -> String
      -> Either ParseError [(String, String)]
parse ps content =
  let parseMap = fromList $ fmap (\(CParser name p) -> (name, p)) ps
  in do
    (name, ls) <- nameAndLines content
    case Data.Map.lookup name parseMap of
      Just p ->
        join <$> mapM p ls
      Nothing ->
        Left $ NoParser name

convert :: [CompanyParser]
        -> String
        -> Either ParseError String
convert ps content = do
  pairs <- parse ps content
  return . unlines $ fmap toCompanyX pairs
