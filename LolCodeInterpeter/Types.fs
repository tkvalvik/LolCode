module Types

open System
open System.Globalization
open AST

exception RuntimeError of string

exception Abort

type Value =
    | String of string
    | Int of int
    | Double of double
    | Bool of bool
    | Null


let stringify value =
    match value with
        | String x -> x
        | Int x -> x.ToString()
        | Double x -> x.ToString( CultureInfo.InvariantCulture )
        | Bool x -> if x then "WIN" else "FAIL"
        | Null -> raise (RuntimeError "Cannot convert NOOB to YARN")

let integerify value = 
    match value with
        | String x -> Int32.Parse(x)
        | Int x -> x
        | Double x -> raise (RuntimeError "Cannot convert Numbar to Numbr")
        | Bool x -> if x then 1 else 0
        | Null -> raise (RuntimeError "Cannot convert NOOB to NUMBR")

let doublify value = 
    match value with
        | String x -> Double.Parse(x, CultureInfo.InvariantCulture )
        | Int x -> double x
        | Double x -> x
        | Bool x -> if x then 1.0 else 0.0
        | Null -> raise (RuntimeError "Cannot convert NOOB to NUMBAR")

let boolify value = 
    match value with
        | Bool x -> x
        | Null -> false
        | _ -> true

let numberify str = 
   match System.Int32.TryParse(str) with
   | (true,x) -> Int x
   | _ -> match System.Double.TryParse(str, NumberStyles.Number, CultureInfo.InvariantCulture) with
          | (true,x) -> Double x
          | _ -> raise (RuntimeError ("Cannot convert " + str + " to numeric."))


let calculateDouble op left right =
    let l = doublify left
    let r = doublify right
    match op with
        | Plus -> l + r
        | Minus -> l - r
        | Div -> l / r
        | Mult -> l * r
        | Greater -> if l > r then l else r
        | Lesser -> if l > r then r else l

let calculateInteger op left right =
    let l = integerify left
    let r = integerify right
    match op with
        | Plus -> l + r
        | Minus -> l - r
        | Div -> l / r
        | Mult -> l * r
        | Greater -> if l > r then l else r
        | Lesser -> if l > r then r else l

let calculateBool op left right =
    let l = boolify left
    let r = boolify right
    match op with
        | Plus -> l || r
        | Greater -> if l > r then l else r
        | Lesser -> if l > r then r else l
        | _ -> raise (RuntimeError "Only addition possible with troof")

let rec calculate op left right =
    match (left, right) with
        | (String x, _) ->  calculate op (numberify x) right 
        | (_, String x) -> calculate op left (numberify x)
        | (Double x, _) | (_, Double x) -> Double ( calculateDouble op left right )
        | (Int x, _) | (_, Int x) -> Int ( calculateInteger op  left right )
        | (Bool x, _) | (_, Bool x)  -> Bool(calculateBool op left right)
        | (Null, Null) -> Null

let rec compare op left right =
    match (left, right) with
        | (Int x, Double y) | (Double y, Int x ) -> compare op (Value.Double (double x) ) (Value.Double y)
        | _ -> match op with
                | ComparisonOperator.Equals -> Bool( left = right)
                | ComparisonOperator.NotEquals -> Bool( not (left = right))

let calcBool op left right = 
    Bool (match op with 
            | Or -> boolify left || boolify right
            | And -> boolify left && boolify right)

let cast value t = 
    match t with
        | T_Yarn -> String (stringify value)
        | T_Numbr -> Int (integerify value)
        | T_Numbar -> Double (doublify value)
        | T_Troof -> Bool (boolify value)
        | T_Noob -> Null