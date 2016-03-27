module String.Addons (upperFirst, upperWords, replace) where

{-| Additional functions for working with Strings

@docs upperFirst, upperWords, replace
-}

import String exposing (uncons, cons, words, join)
import Char exposing (toUpper)
import Regex exposing (regex, escape, HowMany(..))
import Maybe exposing (Maybe(..))
import List


{-| Make a string's first character uppercase

    upperFirst "this is a phrase" == "This is a phare"
    upperFirst "hello, world" == "Hello, world"

-}
upperFirst : String -> String
upperFirst word =
  uncons word
    |> Maybe.map (\( head, tail ) -> cons (toUpper head) tail)
    |> Maybe.withDefault ""


{-| Uppercase the first character of each word in a string

    upperWords "this is a phrase" == "This Is A Phrase"
    upperWords "hello, world" == "Hello, World"

-}
upperWords : String -> String
upperWords ws =
  ws
    |> Regex.replace
        All
        (regex "^([a-z])|\\s+([a-z])")
        (\{ match } -> uppercaseMatch match)


uppercaseMatch : String -> String
uppercaseMatch match =
  match
    |> Regex.replace All (regex "\\w+") (\{ match } -> upperFirst match)


{-| Replace all occurrences of the search string with the substitution string.

    replace "Mary" "Sue" "Hello, Mary" == "Hello, Sue"

-}
replace : String -> String -> String -> String
replace search substitution string =
  string
    |> Regex.replace All (regex (escape search)) (\_ -> substitution)
