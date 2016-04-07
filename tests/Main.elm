module Main (..) where

import AddonsTest exposing (evidence)
import String
import Task
import Console
import ElmTest
import Check.Test


port runner : Signal (Task.Task x ())
port runner =
  Console.run
    <| ElmTest.consoleRunner
    <| Check.Test.evidenceToTest evidence
