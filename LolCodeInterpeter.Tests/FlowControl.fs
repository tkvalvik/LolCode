module LolCodeInterpeter.Tests.FlowControl

open NUnit.Framework
open FsUnit
open TestUtils
open Types

[<Test>] 
let loops() = script "Loops.lol"  |> Map.find "result" |> should equal (Types.Int 4)

[<Test>]
let conditionals() = script "Conditionals.lol"  |> Map.find "result" |> should equal Types.Null