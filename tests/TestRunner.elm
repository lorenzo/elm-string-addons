module TestRunner (..) where

import String.Addons exposing (upperFirst, upperWords)
import String exposing (uncons, fromChar, toUpper)
import Check exposing (Claim, Evidence, suite, claim, that, is, for, true, quickCheck)
import Check.Producer exposing (string, list, filter)
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


evidence : Evidence
evidence =
  suite
    "String.Addons"
    [ upperFirstClaims, upperWordsClaims ]
    |> quickCheck


main =
  ElmTest.elementRunner (Check.Test.evidenceToTest evidence)
