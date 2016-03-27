module String.Addons (upperFirst, upperWords) where

{-| Additional functions for working with Strings

@docs upperFirst
-}

import String exposing (uncons, cons, words, join)
import Char exposing (toUpper)
import Regex exposing (regex, replace, HowMany(..))
import Maybe exposing (Maybe(..))
import List


{-| Make a string's first character uppercase
-}
upperFirst : String -> String
upperFirst word =
  uncons word
    |> Maybe.map (\( head, tail ) -> cons (toUpper head) tail)
    |> Maybe.withDefault ""


{-| Uppercase the first character of each word in a string
-}
upperWords : String -> String
upperWords ws =
  ws
    |> replace
        All
        (regex "^([a-z]+)|\\s+([a-z]+)")
        (\{ match } -> uppercaseMatch match)


uppercaseMatch match =
  match
    |> replace All (regex "\\w+") (\{ match } -> upperFirst match)
