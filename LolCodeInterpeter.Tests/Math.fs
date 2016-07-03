module LolCodeInterpeter.Tests.Math

open NUnit.Framework
open FsUnit
open TestUtils
open Types


[<Test>] 
let assignString () = statement "I HAZ A Cookie ITZ \"Hello World!\"" |> Map.find "Cookie" |> should equal (Types.String "Hello World!")

[<Test>] 
let assignInt () = statement "I HAZ A Cookie ITZ 5" |> Map.find "Cookie" |> should equal (Types.Int 5)

[<Test>] 
let assignFloat () = statement "I HAZ A Cookie ITZ 5.5" |> Map.find "Cookie" |> should equal (Types.Double 5.5)

[<Test>] 
let assignWin () = statement "I HAZ A Cookie ITZ WIN" |> Map.find "Cookie" |> should equal (Types.Bool true)

[<Test>] 
let assignFail () = statement "I HAZ A Cookie ITZ FAIL" |> Map.find "Cookie" |> should equal (Types.Bool false)

[<Test>] 
let assignVar () = statement "I HAZ A Cookie ITZ 10, AnotherCookie R Cookie" |> Map.find "AnotherCookie" |> should equal (Types.Int 10)

[<Test>]
let addIntegers () = statement "num R SUM OF 5 AN 6" |> Map.find "num" |> should equal (Types.Int 11)

[<Test>]
let addFloats () = statement "num R SUM OF 5.5 AN 6.5" |> Map.find "num" |> should equal (Types.Double 12.0)

[<Test>]
let addStrings() = statement "num R SUM OF \"5\" AN \"6\"" |> Map.find "num" |> should equal (Types.Int 11)

[<Test>]
let addStringsWithDecimals() = statement "num R SUM OF \"5.5\" AN \"6.5\"" |> Map.find "num" |> should equal (Types.Double 12.0)

[<Test>]
let addIntToFloat () = statement "num R SUM OF 5 AN 6.5" |> Map.find "num" |> should equal (Types.Double 11.5)

[<Test>]
let addIntToString () = statement "num R SUM OF 5 AN \"6.5\"" |> Map.find "num" |> should equal (Types.Double 11.5)


[<Test>]
let multInt () = statement "num R PRODUKT OF 5 AN 6" |> Map.find "num" |> should equal (Types.Int 30)

[<Test>]
let multFloat () = statement "num R PRODUKT OF 0.5 AN 0.5" |> Map.find "num" |> should equal (Types.Double 0.25)

[<Test>]
let multFloatAndInt () = statement "num R PRODUKT OF 0.5 AN 2" |> Map.find "num" |> should equal (Types.Double 1.0)



[<Test>]
let maxInt () = statement "num R BIGGR OF 5 AN 6" |> Map.find "num" |> should equal (Types.Int 6)

[<Test>]
let maxIntReverse () = statement "num R BIGGR OF 6 AN 5" |> Map.find "num" |> should equal (Types.Int 6)

[<Test>]
let maxFloat () = statement "num R BIGGR OF 5.5 AN 6.5" |> Map.find "num" |> should equal (Types.Double 6.5)

[<Test>]
let maxFloatReverse () = statement "num R BIGGR OF 6.5 AN 5.5" |> Map.find "num" |> should equal (Types.Double 6.5)



[<Test>]
let minInt () = statement "num R SMALLR OF 5 AN 6" |> Map.find "num" |> should equal (Types.Int 5)

[<Test>]
let minIntReverse () = statement "num R SMALLR OF 6 AN 5" |> Map.find "num" |> should equal (Types.Int 5)

[<Test>]
let minFloat () = statement "num R SMALLR OF 5.5 AN 6.5" |> Map.find "num" |> should equal (Types.Double 5.5)

[<Test>]
let minFloatReverse () = statement "num R SMALLR OF 6.5 AN 5.5" |> Map.find "num" |> should equal (Types.Double 5.5)


[<Test>] // 4+(min(6,1)*5)
let compositeExpression () = statement "num R SUM OF 4 AN PRODUKT OF SMALLR OF 6 AN 1 AN 5" |> Map.find "num" |> should equal (Types.Int 9)

[<Test>]
let uppinInt () = statement  "num R 1, UPPIN YR num, UPPIN YR num" |> Map.find "num" |> should equal (Types.Int(3))

[<Test>]
let nerfinInt () = statement  "num R 10, NERFIN YR num, NERFIN YR num" |> Map.find "num" |> should equal (Types.Int(8))


[<Test>]
let compareInt () = statement  "BOTH SAEM 10 AN 10" |> Map.find "IT" |> should equal (Types.Bool(true))

[<Test>]
let compareFloat () = statement  "BOTH SAEM 10.0 AN 10" |> Map.find "IT" |> should equal (Types.Bool(true))

[<Test>]
let compareIntFloat () = statement  "BOTH SAEM 10.0 AN 10.0" |> Map.find "IT" |> should equal (Types.Bool(true))

[<Test>]
let compareString () = statement  "BOTH SAEM \"10.0\" AN \"10.0\"" |> Map.find "IT" |> should equal (Types.Bool(true))

[<Test>]
let compareIntString () = statement  "BOTH SAEM 10 AN \"10\"" |> Map.find "IT" |> should equal (Types.Bool(false))

[<Test>]
let compareBool () = statement  "BOTH SAEM FAIL AN FAIL" |> Map.find "IT" |> should equal (Types.Bool(true))