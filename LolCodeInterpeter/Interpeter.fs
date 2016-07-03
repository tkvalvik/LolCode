module Interpeter

open AST
open Types
open System.IO

let resolveIdentifier (state : Map<string, Value>) identifier =
    Map.find identifier state

let setVariable identifier value state =
    state |> Map.add identifier value

let rec evaluateExpression expression state =   
    match expression with
        | Yarn x -> Types.String (x.Substring(1, x.Length - 2))
        | Numbr x ->  Int x
        | Numbar x -> Types.Double x
        | Troof x -> Bool x
        | Noob -> Null
        | Identifier x -> resolveIdentifier state x
        | Calc (op, left, right) -> calculate op (evaluateExpression left state) (evaluateExpression right state)
        | Boolean (op, left, right) -> calcBool op (evaluateExpression left state) (evaluateExpression right state)
        | Comparison(op, left, right) -> compare op (evaluateExpression left state) (evaluateExpression right state)
        | Concatenate x -> x |> List.map (fun e -> stringify (evaluateExpression e state)) |>  List.reduce(fun acc str -> acc + str) |> String
        | Cast (exp, t) -> cast (evaluateExpression exp state) t

let evaluateUnary exp state = 
        match exp with
            | Numeric (var, i) -> state |> setVariable var (Int( integerify (resolveIdentifier state var) + i))
            
let resolveCoditionalBranch expr trueCase falseCase state =
    let c = boolify (evaluateExpression expr state)
    if c then trueCase else falseCase

let evaluateLoopTerminator term state =
    match term with
        | Wile x -> boolify ( evaluateExpression x state )
        | Til x -> not (boolify (evaluateExpression x state ))
        | LoopTerminator.Empty -> true

let evaluateLoopOperation op state =
    match op with 
    | Operation unary -> evaluateUnary unary state
    | LoopOperator.Empty -> state

let rec evaluateLoop evaluate op term stmnts state  =
    try
        if evaluateLoopTerminator term state then
            state |> evaluate stmnts |> evaluateLoopOperation op |> evaluateLoop evaluate op term stmnts
        else
            state
     with 
        | Abort -> state

let rec evaluate statements state =    
    match statements with
     | [] -> state
     | _ -> 
        match statements.Head with
            | Print expr -> state |> evaluateExpression  expr |> stringify |> printf "%s\n"; state
            | Declaration (id, expr) -> state |> setVariable id (evaluateExpression expr state)
            | Conditional (condition, success, fail) -> state |> evaluate (resolveCoditionalBranch condition success fail state) 
            | Loop(op, term, stmnts) -> state |> evaluateLoop evaluate op term stmnts
            | UnaryStmt(exp) -> state |> evaluateUnary exp
            | Input var -> state |> setVariable var (Value.String (System.Console.ReadLine())) 
            | Break -> raise Abort
            | _ -> state
            |> evaluate statements.Tail
        
let run statements =
     evaluate statements Map.empty