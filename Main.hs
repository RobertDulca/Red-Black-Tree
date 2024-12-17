{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.IO
import Data.Char
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

------------------------------------------------------------
-- Definition
------------------------------------------------------------

data Color = Red | Black deriving (Show, Eq)

data RBTree a = Empty
              | Node Color (RBTree a) a (RBTree a)
              deriving (Show, Eq)

-- | leerer RB-Baum
--
-- >>> inOrder (empty :: RBTree Int)
-- []
empty :: RBTree a
empty = Empty

------------------------------------------------------------
-- Einfügen und Suchen
------------------------------------------------------------

-- | member prüft, ob Element im Baum vorhanden ist.
--
-- Beispiel:
-- >>> let t = foldl' (flip insert) empty ["b","a","c"]
-- >>> member "a" t
-- True
-- >>> member "z" t
-- False
member :: Ord a => a -> RBTree a -> Bool
member _ Empty = False
member x (Node _ l v r)
    | x < v     = member x l
    | x > v     = member x r
    | otherwise = True

-- | insert fügt Element in RB-Baum ein, falls nicht vorhanden ist.
-- Baum bleibt Red-Black-Baum und balanciert
--
-- >>> let t = foldl' (flip insert) empty [3,1,2]
-- >>> inOrder t
-- [1,2,3]
--
-- >>> let t2 = foldl' (flip insert) empty ["c","a","b"]
-- >>> inOrder t2
-- ["a","b","c"]
insert :: Ord a => a -> RBTree a -> RBTree a
insert x s = makeBlack (ins s)
  where
    ins Empty = Node Red Empty x Empty
    ins (Node color a y b)
        | x < y = balance color (ins a) y b
        | x > y = balance color a y (ins b)
        | otherwise = Node color a y b -- schon vorhanden
    makeBlack (Node _ a y b) = Node Black a y b
    makeBlack Empty = Empty

-- | balance stellt sicher, dass die RB-Eigenschaften nach dem Einfügen erhalten bleiben.
balance :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balance Black (Node Red (Node Red a x b) y c) z d =
    Node Red (Node Black a x b) y (Node Black c z d)
balance Black (Node Red a x (Node Red b y c)) z d =
    Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red b y (Node Red c z d)) =
    Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red (Node Red b y c) z d) =
    Node Red (Node Black a x b) y (Node Black c z d)
balance color a x b = Node color a x b

------------------------------------------------------------
-- In-Order Traversal
------------------------------------------------------------

-- | inOrder durchläuft RB-Baum in sortierter Reihenfolge.
-- Bsp:
--
-- >>> inOrder (foldl' (flip insert) empty [2,1,3])
-- [1,2,3]
inOrder :: RBTree a -> [a]
inOrder Empty = []
inOrder (Node _ l v r) = inOrder l ++ [v] ++ inOrder r

------------------------------------------------------------
-- Datenverarbeitung: Tokenisieren, Filtern
------------------------------------------------------------

-- | normalizeText ersetzt alle Nicht-Buchstaben durch Leerzeichen und wandelt
-- alle Buchstaben in Kleinbuchstaben um.
--
-- >>> normalizeText "Antichrist--I"
-- "antichrist  i"
normalizeText :: String -> String
normalizeText = map (\c -> if isAlpha c then toLower c else ' ')

-- | Tokenize: Wandle den ganzen Text um und splitte anhand von Leerzeichen.
-- Beispiel:
-- >>> tokenize "Hello, world! 42 times."
-- ["hello","world","times"]
--
-- >>> tokenize "you--sit"
-- ["you","sit"]
tokenize :: String -> [String]
tokenize = words . normalizeText

------------------------------------------------------------
-- MAIN
------------------------------------------------------------

main :: IO ()
main = do
    -- Datei lesen
    content <- readFile "war_and_peace.txt"
    
    -- Tokenisierung
    let wordsList = tokenize content
    
    -- In Red-Black-Baum einfügen, nur wenn Wort noch nicht vorhanden
    let tree = foldl' (\acc w -> if member w acc then acc else insert w acc) empty wordsList

    -- In-Order Traversal für sortierte Wortliste
    let sortedWords = inOrder tree

    -- In output.txt schreiben
    withFile "output.txt" WriteMode $ \h -> do
        mapM_ (hPutStrLn h) sortedWords

    putStrLn "Fertig! Die sortierten Wörter wurden in output.txt geschrieben."