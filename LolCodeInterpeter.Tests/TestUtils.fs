module LolCodeInterpeter.Tests.TestUtils

open LolCode
open System.Reflection
open System.IO


let statement stmnt = 
    runScript ("HAI, " + stmnt + ", KTHXBYE")

let script name = 
    use stream = new StreamReader( Assembly.GetExecutingAssembly().GetManifestResourceStream(name))
    runScript (stream.ReadToEnd())
    