module TestRunner (..) where

import String.Addons exposing (upperFirst, upperWords, replace)
import String exposing (uncons, fromChar, toUpper)
import Check exposing (Claim, Evidence, suite, claim, that, is, for, true, false, quickCheck)
import Check.Producer exposing (string, list, tuple, filter)
import Check.Test
import ElmTest
import Debug


upperFirstClaims : Claim
upperFirstClaims =
  suite
    "upperFirst"
    [ claim
        "It only converts to uppercase the first char in the string"
        `that` (\string -> uncons (upperFirst string) |> Maybe.map fst |> Maybe.map fromChar |> Maybe.withDefault "")
        `is` (\string -> uncons string |> Maybe.map fst |> Maybe.map fromChar |> Maybe.map toUpper |> Maybe.withDefault "")
        `for` string
    , claim
        "The tail of the stirng remains untouched"
        `that` (\string -> uncons (upperFirst string) |> Maybe.map snd |> Maybe.withDefault "")
        `is` (\string -> uncons string |> Maybe.map snd |> Maybe.withDefault "")
        `for` string
    ]


upperWordsClaims : Claim
upperWordsClaims =
  suite
    "upperWords"
    [ claim
        "It converts the first letter of each word to uppercase"
        `that` (\arg -> arg |> String.join " " |> upperWords |> String.words)
        `is` (\arg -> arg |> String.join " " |> String.words |> List.map upperFirst)
        `for` list string
    , claim
        "It does not change the length of the string"
        `that` (\arg -> arg |> String.join " " |> upperWords |> String.length)
        `is` (\arg -> arg |> String.join " " |> String.length)
        `for` list string
    ]


replaceClaims : Claim
replaceClaims =
  suite
    "replace"
    [ claim
        "It substitues all occurences of the same sequence"
        `that` (\( string, substitute ) -> replace string substitute string)
        `is` (\( string, substitute ) -> substitute)
        `for` tuple ( string, string )
    , claim
        "It substitutes multiple occurances"
        `false` (\string -> replace "a" "b" string |> String.contains "a")
        `for` filter (\arg -> String.contains "a" arg) string
    , claim
        "It substitutes multiple occurances"
        `true` (\string -> replace "\\" "bbbbb" string |> String.contains "bbbb")
        `for` filter (\arg -> String.contains "\\" arg) string
    ]


evidence : Evidence
evidence =
  suite
    "String.Addons"
    [ upperFirstClaims
    , upperWordsClaims
    , replaceClaims
    ]
    |> quickCheck


main =
  ElmTest.elementRunner (Check.Test.evidenceToTest evidence)
