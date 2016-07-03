module AST

type CalcOperator =
    | Plus
    | Minus
    | Mult
    | Div
    | Greater
    | Lesser

type BooleanOperator =
    | Or
    | And

type ComparisonOperator =
    | Equals
    | NotEquals

type Type = 
    | T_Numbar
    | T_Numbr
    | T_Yarn
    | T_Troof
    | T_Noob
    
type Unary = 
    | Numeric of string * int

type Expression = 
    | Yarn of string
    | Numbr of int
    | Numbar of double
    | Identifier of string
    | Troof of bool
    | Noob
    | Calc of CalcOperator * Expression * Expression
    | Boolean of BooleanOperator * Expression * Expression
    | Comparison of ComparisonOperator * Expression * Expression
    | Concatenate of Expression list
    | Cast of Expression * Type
    

type LoopOperator =
    | Operation of Unary
    | Empty
    

type LoopTerminator =
    | Til of Expression
    | Wile of Expression
    | Empty


type Statement =
    | Print of Expression
    | Declaration of Name:string * Value : Expression
    | Conditional of Expr:Expression * Win : Statement list *  Fail : Statement list
    | Loop of LoopOperator * LoopTerminator * Statement list
    | UnaryStmt of Unary 
    | Input of string
    | Switch of Statement list
    | Break
    | Empty
    


let reverseAndCleanStatements statements = 
   statements |> List.filter (fun x -> match x with | Empty -> false | _ -> true) |> List.rev