------------------------------------------------------------------------------ 
--- Library for converting ground terms to strings and vice versa. 
--- 
--- @author Michael Hanus 
--- @version March 2021
------------------------------------------------------------------------------ 
{-# LANGUAGE CPP #-}

module ReadShowTerm
 ( showTerm, showQTerm, readQTerm, readsQTerm
 , readsUnqualifiedTerm, readUnqualifiedTerm
 , readQTermFile, readQTermListFile
 , writeQTermFile, writeQTermListFile
 ) where

import Data.Char(isSpace)

--- Transforms a ground(!) term into a string representation 
--- in standard prefix notation. 
--- Thus, showTerm suspends until its argument is ground. 
--- This function is similar to the prelude function `show` 
--- but can read the string back with `readUnqualifiedTerm` 
--- (provided that the constructor names are unique without the module 
--- qualifier). 
#ifdef __CURRY2GO__
showTerm :: Show a => a -> String
#else
showTerm :: _ -> String 
#endif
showTerm x = prim_showTerm $## x 
 
#ifdef __CURRY2GO__
prim_showTerm :: Show a => a -> String 
prim_showTerm = show
#else
prim_showTerm :: _ -> String 
prim_showTerm external 
#endif

 
--- Transforms a ground(!) term into a string representation 
--- in standard prefix notation. 
--- Thus, showTerm suspends until its argument is ground. 
--- Note that this function differs from the prelude function `show` 
--- since it prefixes constructors with their module name 
--- in order to read them back with `readQTerm`. 
#ifdef __CURRY2GO__
showQTerm :: Show a => a -> String
#else
showQTerm :: _ -> String
#endif
showQTerm x = prim_showQTerm $## x 

#ifdef __CURRY2GO__
prim_showQTerm :: Show a => a -> String
prim_showQTerm = show
#else
prim_showQTerm :: _ -> String 
prim_showQTerm external 
#endif

--- Transform a string containing a term in standard prefix notation 
--- without module qualifiers into the corresponding data term. 
--- The first argument is a non-empty list of module qualifiers
--- that are tried to prefix the constructor in the string
--- in order to get the qualified constructors 
--- (that must be defined in the current program!). 
--- In case of a successful parse, the result is a one element list 
--- containing a pair of the data term and the remaining unparsed string. 
 
#ifdef __CURRY2GO__
readsUnqualifiedTerm :: Read a => [String] -> String -> [(a,String)]
#else
readsUnqualifiedTerm :: [String] -> String -> [(_,String)]
#endif
readsUnqualifiedTerm [] _ =
  error "ReadShowTerm.readsUnqualifiedTerm: list of module prefixes is empty"
readsUnqualifiedTerm (prefix:prefixes) s =
  readsUnqualifiedTermWithPrefixes (prefix:prefixes) s
 
#ifdef __CURRY2GO__
readsUnqualifiedTermWithPrefixes :: Read a => [String] -> String -> [(a,String)]
#else
readsUnqualifiedTermWithPrefixes :: [String] -> String -> [(_,String)]
#endif
readsUnqualifiedTermWithPrefixes prefixes s =
  (prim_readsUnqualifiedTerm $## prefixes) $## s 
 
#ifdef __CURRY2GO__
prim_readsUnqualifiedTerm :: Read a => [String] -> String -> [(a,String)]
prim_readsUnqualifiedTerm _ s = reads s
#else
prim_readsUnqualifiedTerm :: [String] -> String -> [(_,String)]
prim_readsUnqualifiedTerm external
#endif
 
--- Transforms a string containing a term in standard prefix notation 
--- without module qualifiers into the corresponding data term. 
--- The first argument is a non-empty list of module qualifiers
--- that are tried to prefix the constructor in the string
--- in order to get the qualified constructors 
--- (that must be defined in the current program!). 
--- 
--- Example: `readUnqualifiedTerm ["Prelude"] "Just 3"` evaluates to `(Just 3)`
 
#ifdef __CURRY2GO__
readUnqualifiedTerm :: Read a => [String] -> String -> a
#else
readUnqualifiedTerm :: [String] -> String -> _
#endif
readUnqualifiedTerm prefixes s = case result of
  [(term,tail)] 
     -> if all isSpace tail then term 
        else error ("ReadShowTerm.readUnqualifiedTerm: no parse, unmatched string after term: "++tail) 
  [] ->  error "ReadShowTerm.readUnqualifiedTerm: no parse" 
  _  ->  error "ReadShowTerm.readUnqualifiedTerm: ambiguous parse" 
 where result = readsUnqualifiedTerm prefixes s 
 
--- Transforms a string containing a term in standard prefix notation 
--- with qualified constructor names into the corresponding data term. 
--- In case of a successful parse, the result is a one element list 
--- containing a pair of the data term and the remaining unparsed string. 
 
#ifdef __CURRY2GO__
readsQTerm :: Read a => String -> [(a,String)]
#else
readsQTerm :: String -> [(_,String)]
#endif
readsQTerm s = prim_readsQTerm $## s
 
#ifdef __CURRY2GO__
prim_readsQTerm :: Read a => String -> [(a,String)]
prim_readsQTerm = reads
#else
prim_readsQTerm :: String -> [(_,String)]
prim_readsQTerm external
#endif
 
--- Transforms a string containing a term in standard prefix notation 
--- with qualified constructor names into the corresponding data term. 
 
#ifdef __CURRY2GO__
readQTerm :: Read a => String -> a
#else
readQTerm :: String -> _
#endif
readQTerm s = case result of 
  [(term,tail)] -> if all isSpace tail
                     then term 
                     else error "ReadShowTerm.readQTerm: no parse" 
  [] ->  error "ReadShowTerm.readQTerm: no parse" 
  _  ->  error "ReadShowTerm.readQTerm: ambiguous parse" 
 where result = readsQTerm s 
 
--- Reads a file containing a string representation of a term 
--- in standard prefix notation and returns the corresponding data term. 
 
#ifdef __CURRY2GO__
readQTermFile :: Read a => String -> IO a
#else
readQTermFile :: String -> IO _
#endif
readQTermFile file = readFile file >>= return . readQTerm 
 
--- Reads a file containing lines with string representations of terms 
--- of the same type and returns the corresponding list of data terms. 
 
#ifdef __CURRY2GO__
readQTermListFile :: Read a => String -> IO [a]
#else
readQTermListFile :: String -> IO [_]
#endif
readQTermListFile file = readFile file >>= return . map readQTerm . lines 

 
--- Writes a ground term into a file in standard prefix notation. 
--- @param filename - The name of the file to be written. 
--- @param term - The term to be written to the file as a string. 
 
#ifdef __CURRY2GO__
writeQTermFile :: Show a => String -> a -> IO () 
#else
writeQTermFile :: String -> _ -> IO () 
#endif
writeQTermFile filename term = writeFile filename (showQTerm term) 
 
--- Writes a list of ground terms into a file. 
--- Each term is written into a separate line which might be useful 
--- to modify the file with a standard text editor. 
--- @param filename - The name of the file to be written. 
--- @param terms - The list of terms to be written to the file. 
 
#ifdef __CURRY2GO__
writeQTermListFile :: Show a => String -> [a] -> IO () 
#else
writeQTermListFile :: String -> [_] -> IO () 
#endif
writeQTermListFile filename terms = 
    writeFile filename (unlines (map showQTerm terms)) 
 
