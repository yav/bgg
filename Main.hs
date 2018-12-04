{-# Language RecordWildCards #-}
module Main(main) where

import Text.XML.Light
import Text.XML.Light.Input
import Text.XML.Light.Proc
import Data.Maybe(mapMaybe)
import Data.Char(isDigit,isSpace)


main :: IO ()
main =
  do txt <- readFile "out.xml"
     let xml = onlyElems (parseXML txt)
         items = concatMap (filterElements ((== unqual "item") . elName)) xml
     mapM_ (putStrLn . ppItem) $ mapMaybe parseItem items


data Item = Item
  { gameId :: String
  , gameName :: String
  , gameDescription :: String
  , gameComments :: [String]
  } deriving Show

parseItem :: Element -> Maybe Item
parseItem el =
  do gameId <- attr "objectid"
     gameName <- attr "objectname"
     body <- findElement (unqual "body") el
     let gameDescription = strContent body
         cs = findChildren (unqual "comment") el
         gameComments = map strContent cs
     pure Item { .. }

  where
  as = elAttribs el
  attr a = lookupAttr (unqual a) as

ppItem :: Item -> String
ppItem Item { .. } = unlines $
  [ "* " ++ gameName  ++ ", " ++ unwords (findPrice gameDescription)] ++
  [ "  " ++ x | x <- lines gameDescription ] ++
  concat [ block "    - " "      " x | x <- gameComments ]
  where
  block p1 pmore xs = case dropWhile (all isSpace) (lines xs) of
                        [] -> []
                        a : as -> (p1 ++ a) : map (pmore ++) as

findPrice :: String -> [String]
findPrice x = case break (== '$') x of
                 (_,_:xs) -> takeWhile isDigit xs : findPrice (dropWhile isDigit xs)
                 _ -> []
