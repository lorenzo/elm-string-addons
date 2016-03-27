module String.Addons (..) where

{-| Additional functions for working with Strings
-}

import String exposing (uncons, cons)
import Char exposing (toUpper)
import Maybe exposing (Maybe(..))

upperFirst: String -> String
upperFirst word =
    let split = uncons word in
    case split of
        Nothing -> ""
        Just (head, tail) -> cons (toUpper head) tail
