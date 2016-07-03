module LolCodeInterpeter.Tests.Casting

open NUnit.Framework
open FsUnit
open TestUtils
open Types


[<Test>] 
let castFloat () = statement "num R MAEK 10 A NUMBAR" |> Map.find "num" |> should equal (Types.Double 10.0)

[<Test>] 
let castInt () = statement "num R MAEK \"10\" A NUMBR" |> Map.find "num" |> should equal (Types.Int 10)

[<Test>] 
let castString () = statement "num R MAEK 10 A YARN" |> Map.find "num" |> should equal (Types.String "10")

[<Test>] 
let castIntToStringVar () = statement "num R 10, num IS NOW A YARN" |> Map.find "num" |> should equal (Types.String "10")

[<Test>] 
let castIntToFloatVar () = statement "num R 10, num IS NOW A NUMBAR" |> Map.find "num" |> should equal (Types.Double 10.0)

[<Test>] 
let castBoolToStringVar () = statement "num R WIN, num IS NOW A YARN" |> Map.find "num" |> should equal (Types.String "WIN")