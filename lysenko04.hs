{-# OPTIONS_GHC -Wall #-}
module Lysenko04 where

-- lab04 concerning XML PARSING

import Data.Char

type Name = String
type Attributes = [(Name, String)]
data XML = Text String | Element Name Attributes [XML]
         deriving (Eq, Show)
type Stack = [XML]

{-
--example
printXML (Element "a" [("href", "http://wikipedia.org")] [Text "link to wiki"])
-}

-- class work
cntElement :: XML -> Int
cntElement (Text _) = 0
cntElement (Element _ _ children) = (+1) . sum $ map cntElement children

-- getters
-- TODO delete
getAllChildren :: XML -> [XML]
getAllChildren (Text _) = []
getAllChildren (Element _ _ children) = children

getText :: XML -> String
getText (Text txt) = txt
getText (Element _ _ _) = ""

getName :: XML -> String
getName (Text _) = ""
getName (Element name _ _) = name

-- Задача 1 -----------------------------------------
skipSpace :: String -> String
skipSpace [] = []
skipSpace s@(x:xs) | isSpace x = skipSpace xs
                   | otherwise = s
{-
test:
skipSpace "\n \n\nsome \n \n text" == "some \n \n text"
skipSpace "\t    text\t \n"
skipSpace "  \n  text\t \n"
-}
-- Задача 2 -----------------------------------------
getAttribute :: String -> XML -> String
getAttribute _ (Text _) = ""
getAttribute name (Element _ atts _) =
  let
    tuples = filter ((== name).fst) atts
  in if not$null tuples
    then snd$head tuples
    else ""
{-
test:
getAttribute "x" x2 == "1"
getAttribute "x" (Text "t") == ""
-}
-- Задача 3 -----------------------------------------
getChildren :: String -> XML -> [XML]
getChildren _ (Text _) = []
getChildren name (Element _ _ children) = filter ((== name).getName) children
{-
test
getChildren "b" x2 ==  [Element "b" [] [Text "A"],Element "b" [] [Text "B"]]
getChildren "c" x2 == []
-}

-- Задача 4 -----------------------------------------
getChild :: String -> XML -> XML
getChild name xml = let children = getChildren name xml
                    in if not$null children
                       then head children
                       else Text ""
{-
--test
getChild "b" x2 == Element "b" [] [Text "A"]
getChild "c" x2 == Text ""
-}
-- Задача 5 -----------------------------------------
addChild :: XML -> XML -> XML
-- Передумова: другий аргумент - завжди побудований конструктором Element
addChild _ (Text _) = error "incompatible type"
addChild xml1 (Element name atts children) =
    (Element name atts (children ++ [xml1]))
{-
--test
addChild (Text "B") (Element "a" [] [Text "A"]) == Element "a" [] [Text "A",Text "B"]
-}
-- Задача 6 -----------------------------------------
getValue :: XML -> XML
getValue t@(Text _) = t
getValue (Element _ _ []) = Text ""
getValue (Element _ _ children) =
    let txt = foldl (++) "" $ map (getText.getValue) children
    in Text txt

{-
-- concatenate list of strings:
foldl (++) "" ["aaa", "bbb", "ccc"]
--test
getValue x1 == Text "A"
getValue x2 == Text "AB"
-}

-- Задача 7 -----------------------------------------
addText :: String -> Stack -> Stack
-- Передумова: Є по крайній мірі один елемент Element в стеку
addText _ [] = error "unsupported operation"
addText s (x:xs) = (addChild (Text s) x) : xs
{-
--test
let tmpTxt = [Element "a" [] [Text "A",Text "B"], Element "" [] []]
addText  "B" [Element "a" [] [Text "A"], sentinel]  == tmpTxt
-}

-- Задача 8 -----------------------------------------
popAndAdd :: Stack -> Stack
-- Передумова: Є по крайній мірі два елемента Elements в стеку
popAndAdd [] = error "unsupporded operation"
popAndAdd (_:[]) = error "unsupporded operation"
popAndAdd (x:(y:list)) = addChild x y : list
{-
--test
let tmpElt = [Element "ab" [("a","1")] [Element "a" [] [Text "A"]], Element "" [] []]
popAndAdd [Element "a" [] [Text "A"], Element "ab" [("a","1")] [], sentinel] == tmpElt
-}

-- Початковий елемент стеку
sentinel :: XML
sentinel = Element "" [] []

-- Задача 9 -----------------------------------------
parseAttributes :: String -> (Attributes, String)
-- Передумова: Рядок, що містить XML-атрибути, синтаксично вірний
parseAttributes [] = ([], "")
parseAttributes s =
  let
    tuple = splitBy s '>'
    params = (parseName.skipSpace.fst) tuple
    txt = snd tuple
    cnt = length $ filter (== '=') $ snd params
    fltr = filter (\c -> notElem c "/\"= ")
    twiceSplitted = (splitBy (snd $ splitBy (snd params) '\"') '\"')
  in if cnt > 1
     then ((fst params, fltr $ fst twiceSplitted) :
              (fst $ parseAttributes $ snd twiceSplitted), txt)
     else ([(fst params, fltr $ snd params)], txt)
{-
--test
parseAttributes "x=\"7\">rest of text"

parseAttributes "x=\"7\">rest of text" == ([("x","7")],"rest of text")
let tmpAttr = ([("a","0"),("b","1")],"rest of text")
parseAttributes "a = \"0\" b = \"1\" >rest of text" == tmpAttr
-}

-- note: we can also can use break instead of writing the function below
-- splits str by sep-arator

splitBy :: String -> Char -> (String, String)
splitBy str sep =
  let
    tuple = break (== sep) str
    val2 = if (null.snd) tuple then (snd tuple) else ((tail.snd) tuple)
  in (fst tuple, val2)

{-
--test
splitBy "hello>world" '>'
splitBy "" '>'
-}
-- Аналіз імені елемента/атрибута
parseName :: String -> (Name, String)
parseName []
  = error "Error: attempt to read empty name"
parseName s@(c1 : _)
  | isAlpha c1 = break (not . isNameChar) s
  | otherwise = error ("parseName error: name " ++ show s ++
                      " must begin with a letter")
  where
    isNameChar c = isAlpha c || isDigit c || elem c "-."
{-
parseName "a = \"0\" b = \"1\" >rest of text"
-}

-- Задача 10 -----------------------------------------
parse :: String -> XML
-- Передумова: рядок, що містить XML-документ, синтаксично вірний
parse s = parse' (skipSpace s) [sentinel]

parse' :: String -> Stack -> XML
parse' _ _ = Element "" [] []

{-
--test
parse s1 == Element "a" [] [Text "A"]
parse s1 == x1
parse s2 == x2
parse s3 == x3
parse films == filmsParsed
parse casablanca == casablancaParsed
-}
-----------------------------------------------------------------------
-- Деякі корисні функції перетворення в рядок і виводу
-- Функція перетворення в рядок ('show' function) для XML об'єктів
showXML :: XML -> String
showXML (Text t) = t
showXML (Element n as es)
     = "<" ++ n ++ showAtts as ++ ">" ++ concatMap showXML es ++ "</" ++ n ++ ">"
       where
          showAtts ast = concatMap showAtt ast
          showAtt (n1, v) = " " ++ n1 ++ "=" ++ "\"" ++ v ++ "\""
-- Функція перетворення в рядок ('show' function) для списку XML об'єктів
showXMLs :: [XML] -> String
showXMLs = concatMap showXML
-- Функція виводу XML об'єкта на екран
printXML :: XML -> IO()
printXML = putStrLn . showXML

-------------------------------------------------------------------------
-- Тестові дані
-- Прості тести XML-об'єктів (без проміжків)
s1, s2, s3 :: String
s1 = "<a>A</a>"
s2 = "<a x=\"1\"><b>A</b><b>B</b></a>"
s3 = "<a>\
      \<b>\
        \<c att=\"att1\">text1</c>\
        \<c att=\"att2\">text2</c>\
      \</b>\
      \<b>\
        \<c att=\"att3\">text3</c>\
        \<d>text4</d>\
      \</b>\
    \</a>"
-- Результати аналізу попередніх XML-об'єктів
x1, x2, x3 :: XML
x1 = Element "a" [] [Text "A"]
x2 = Element "a"
            [("x","1")]
            [Element "b" [] [Text "A"],
             Element "b" [] [Text "B"]]
x3 = Element "a"
            []
            [Element "b"
                     []
                     [Element "c"
                              [("att","att1")]
                              [Text "text1"],
                      Element "c"
                              [("att","att2")]
                              [Text "text2"]],
             Element "b"
                     []
                     [Element "c"
                              [("att","att3")]
                              [Text "text3"],
                      Element "d"
                              []
                              [Text "text4"]]]

casablanca :: String
casablanca
  = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942\
    \</year>\n</film>\n\n\n"

casablancaParsed :: XML
casablancaParsed
  = Element "film"
            [("title","Casablanca")]
            [Text "\n  ",
             Element "director" [] [Text "Michael Curtiz"],
             Text "\n  ",
             Element "year" [] [Text "1942"],
             Text "\n"]

-- XML-документ з Мал.1
films :: String
films
  = "<filmlist>\n\
    \  <film title = \"Rear Window\">\n\
    \    <director>Alfred Hitchcock</director>\n\
    \    <composer>Franz Waxman</composer>\n\
    \    <year>1954</year>\n\
    \  </film>\n\
    \  <film   title =  \"2001: A Space Odyssey\">\n\
    \    <director>Stanley Kubrick</director>\n\
    \    <composer>Richard Strauss</composer>\n\
    \    <composer>Gyorgy Ligeti</composer>\n\
    \    <composer>Johann Strauss</composer>\n\
    \    <year>1968</year>\n\
    \  </film>\n\
    \  <film title=\"Lawrence of Arabia\"  >\n\
    \    <duration>228</duration>\n\
    \    <director>David Lean</director>\n\
    \    <composer>Maurice Jarre</composer>\n\
    \  </film>\n\
    \</filmlist>\n\n\n"

-- Результат аналізу  попереднього докуменнту ('parse films')
filmsParsed :: XML
filmsParsed
  = Element "filmlist"
            []
            [Text "\n  ",
             Element "film" [("title","Rear Window")]
                            [Text "\n    ",
                             Element "director" [] [Text "Alfred Hitchcock"],
                             Text "\n    ",
                             Element "composer" [] [Text "Franz Waxman"],
                             Text "\n    ",
                             Element "year" [] [Text "1954"],
                             Text "\n  "],
             Text "\n  ",
             Element "film" [("title","2001: A Space Odyssey")]
                            [Text "\n    ",
                             Element "director" [] [Text "Stanley Kubrick"],
                             Text "\n    ",
                             Element "composer" [] [Text "Richard Strauss"],
                             Text "\n    ",
                             Element "composer" [] [Text "Gyorgy Ligeti"],
                             Text "\n    ",
                             Element "composer" [] [Text "Johann Strauss"],
                             Text "\n    ",
                             Element "year" [] [Text "1968"],
                             Text "\n  "],
             Text "\n  ",
             Element "film" [("title","Lawrence of Arabia")]
                            [Text "\n    ",
                             Element "duration" [] [Text "228"],
                             Text "\n    ",
                             Element "director" [] [Text "David Lean"],
                             Text "\n    ",
                             Element "composer" [] [Text "Maurice Jarre"],
                             Text "\n  "],
             Text "\n"]
