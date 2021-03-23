------------------------------------------------------------------------------ 
--- Library for converting ground terms to strings and vice versa. 
--- 
--- @author Michael Hanus 
--- @version March 2021
------------------------------------------------------------------------------ 
{-# LANGUAGE CPP #-}

module ReadShowTerm ( showTerm, readsUnqualifiedTerm, readUnqualifiedTerm )
 where

import Data.Char ( isSpace )

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
     -> if all isSpace tail
          then term
          else error $ "ReadShowTerm.readUnqualifiedTerm: no parse, " ++
                       "unmatched string after term: " ++ tail
  [] ->  error "ReadShowTerm.readUnqualifiedTerm: no parse"
  _  ->  error "ReadShowTerm.readUnqualifiedTerm: ambiguous parse"
 where
  result = readsUnqualifiedTerm prefixes s
