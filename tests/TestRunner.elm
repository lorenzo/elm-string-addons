module TestRunner (..) where

import String.Addons exposing (upperFirst)
import String exposing (uncons, fromChar, toUpper)
import Check exposing (Claim, Evidence, suite, claim, that, is, for, true, quickCheck)
import Check.Producer exposing (string)
import Check.Test
import ElmTest


claims : Claim
claims =
  suite
    "upperFirst"
    [ claim
        "it only converts to uppercase the first char in the string"
        `that` (\string -> uncons (upperFirst string) |> Maybe.map fst |> Maybe.map fromChar |> Maybe.withDefault "")
        `is` (\string -> uncons string |> Maybe.map fst |> Maybe.map fromChar |> Maybe.map toUpper |> Maybe.withDefault "")
        `for` string
    , claim
        "The tail of the stirng remains untouched"
        `that` (\string -> uncons (upperFirst string) |> Maybe.map snd |> Maybe.withDefault "")
        `is` (\string -> uncons string |> Maybe.map snd |> Maybe.withDefault "")
        `for` string
    ]


evidence : Evidence
evidence =
  quickCheck claims


main =
  ElmTest.elementRunner (Check.Test.evidenceToTest evidence)
