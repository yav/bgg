{-# Language OverloadedStrings #-}
module Main where

import Data.Maybe(fromMaybe)
import Data.Char
import Data.List
import Text.XML.Light
import Text.PrettyPrint
import Data.Ord

main :: IO ()
main = interact
     ( (++"\n")
     . show
     . vsep
     . map snd
     . sortBy (comparing fst)
     . map renderItem
     . concatMap (findChildren (name "item"))
     . onlyElems
     . parseXML
     )

name :: String -> QName
name x = blank_name { qName = x }

vsep :: [Doc] -> Doc
vsep = vcat . intersperse " "

getAttr :: Element -> String -> Doc
getAttr el x = text (fromMaybe "?" (findAttr (name x) el))


renderItem :: Element -> (String,Doc)
renderItem el = (show nm, txt)

  where
  nm = getAttr el "objectname"
  txt = quotes nm <+> "by" <+> getAttr el "username"
        <+> url el $$
            nest 2 (vcat $ [ "*" <+> getText c
                           | c <- findChildren (name "body") el
                           ] ++
                           [ "*" <+> renderComment c
                           | c <- findChildren (name "comment") el
                           ])

(<.>) :: Doc -> Doc -> Doc
(<.>) = (Text.PrettyPrint.<>)

url :: Element -> Doc
url el = hcat [ "https://boardgamegeek.com/geeklist/146056/item/",
                                                          n, "#item", n ]
  where n = getAttr el "id"

renderComment :: Element -> Doc
renderComment el =
  getAttr el "username" <.> ":" <+> getText el

getText = vcat
        . map text
        . dropWhile (all isSpace)
        . lines
        . concatMap cdData
        . onlyText
        . elContent

