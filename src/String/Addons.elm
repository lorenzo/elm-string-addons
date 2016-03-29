module String.Addons (toUpperFirst, toUpperWords, replace, replaceSlice) where

{-| Additional functions for working with Strings

## Modifying

@docs toUpperFirst, toUpperWords

## Replacing

@docs replace, replaceSlice
-}

import String exposing (uncons, cons, words, join)
import Char exposing (toUpper)
import Regex exposing (regex, escape, HowMany(..))
import Maybe exposing (Maybe(..))
import List


{-| Make a string's first character uppercase

    toUpperFirst "this is a phrase" == "This is a phare"
    toUpperFirst "hello, world" == "Hello, world"

-}
toUpperFirst : String -> String
toUpperFirst word =
  uncons word
    |> Maybe.map (\( head, tail ) -> cons (toUpper head) tail)
    |> Maybe.withDefault ""


{-| Uppercase the first character of each word in a string

    toUpperWords "this is a phrase" == "This Is A Phrase"
    toUpperWords "hello, world" == "Hello, World"

-}
toUpperWords : String -> String
toUpperWords ws =
  ws
    |> Regex.replace
        All
        (regex "^([a-z])|\\s+([a-z])")
        (\{ match } -> uppercaseMatch match)


uppercaseMatch : String -> String
uppercaseMatch match =
  match
    |> Regex.replace All (regex "\\w+") (.match >> toUpperFirst)


{-| Replace all occurrences of the search string with the substitution string.

    replace "Mary" "Sue" "Hello, Mary" == "Hello, Sue"

-}
replace : String -> String -> String -> String
replace search substitution string =
  string
    |> Regex.replace All (regex (escape search)) (\_ -> substitution)


{-| Replace text within a portion of a string given a substitution
string, a start index and an end index.

    replaceSlice "Sue" 4 6 "Hi, Bob" == "Hi, Sue"
    replaceSlice "elephants" 0  6 "snakes on a plane!" == "elephats on a plane!"
    replaceSliceslice "under" 7  9 "snakes on a plane!" == "snakes under a plane!"

-}
replaceSlice : String -> Int -> Int -> String -> String
replaceSlice substitution start end string =
  (String.slice 0 start string) ++ substitution ++ (String.slice end (String.length string) string)
