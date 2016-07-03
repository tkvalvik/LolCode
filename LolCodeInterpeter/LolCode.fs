module LolCode

open Interpeter
open System.IO
open Microsoft.FSharp.Text.Lexing


let runScript script = 
    let lexbuf = LexBuffer<char>.FromString script
    let ast = Parser.start Lexer.tokenstream lexbuf    
    
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let result = run ast 
    printfn "%f" stopWatch.Elapsed.TotalMilliseconds
    stopWatch.Stop()
    result

let testFile = File.ReadAllText(Path.Combine(@"C:\Test\FsLexYaccSamples\LolCodeInterpeter", "simpleTest.lol"))
let finalState =  runScript testFile

System.Console.ReadLine() |> ignore
