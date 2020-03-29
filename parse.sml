datatype terminal
  = TLBracket
  | TRBracket
  | TLBrace
  | TRBrace
  | TColon
  | TComma
  | TNull
  | TBoolean of bool
  | TNumber of real
  | TString of string

datatype value
  = JNumber of real
  | JBoolean of bool
  | JNull
  | JString of string
  | JArray of value array
  | JObject of (string * value) list

exception ParseError

fun element (TLBrace :: tr) = object tr
  | element (TLBracket :: tr) = array tr
  | element (TNull :: tr) = tr
  | element (TBoolean b :: tr) = tr
  | element (TNumber n :: tr) = tr
  | element (TString s :: tr) = tr
  | element _ = raise ParseError

and object (TString key :: TColon :: tr) =
    (case (element tr) of
          TComma :: tt => object tt
        | TRBrace :: tt => tt
        | _ => raise ParseError)
  | object (TRBrace :: tr) = tr
  | object _ = raise ParseError

and array (TRBracket :: ts) = ts
  | array ts = (case (element ts) of
                     TComma :: tr => array tr
                   | TRBracket :: tr => tr
                   | _ => raise ParseError)

fun parse ts =
  case (element ts) of
       [] => ()
     | _ => raise ParseError

val () = parse [TLBrace, TString "key", TColon, TNumber 1.0, TRBrace]
