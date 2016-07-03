module Program

open System.IO
open LolCode

let testFile = File.ReadAllText(Path.Combine(@"C:\Test\FsLexYaccSamples\LolCodeInterpeter", "simpleTest.lol"))
let finalState =  runScript testFile

System.Console.ReadLine() |> ignore


