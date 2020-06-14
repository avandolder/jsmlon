datatype terminal
  = TLBracket
  | TRBracket
  | TLBrace
  | TRBrace
  | TColon
  | TComma
  | TNull
  | TBool of bool
  | TNumber of real
  | TString of string

structure Value = struct
  datatype value
    = Number of real
    | Bool of bool
    | Null
    | String of string
    | Array of value list
    | Object of (string * value) list

  fun toString (Number n) = Real.toString n
    | toString (Bool b) = Bool.toString b
    | toString (String s) = "\"" ^ s ^ "\""
    | toString Null = "null"
    | toString (Array a) =
        "[" ^ (String.concatWith "," (List.map toString a)) ^ "]"
    | toString (Object obj) =
        let
          fun join (k, v) = "\"" ^ k ^ "\": " ^ (toString v)
        in
          "{" ^ (String.concatWith "," (List.map join obj)) ^ "}"
        end

  fun prettyString v =
    let
      fun tab n = String.implode (List.tabulate (n, fn _ => #"\t"))
      fun str _ (Array []) = "[]"
        | str t (Array [v]) = "[" ^ (str t v) ^ "]"
        | str t (Array a) =
            let
              val tabs = tab t
              val itabs = tabs ^ "\t"
            in
              "[\n" ^ itabs ^
              (String.concatWith (",\n" ^ itabs)
                (List.map (str (t + 1)) a)) ^
              "\n" ^ tabs ^ "]"
            end
        | str _ (Object []) = "{}"
        | str t (Object obj) =
            let
              val tabs = tab t
              val itabs = tabs ^ "\t"
              fun join t (k, v) = "\"" ^ k ^ "\": " ^ (str t v)
            in
              "{\n" ^ itabs ^
              (String.concatWith (",\n" ^ itabs)
                (List.map (join (t + 1)) obj)) ^
              "\n" ^ tabs ^ "}"
            end
        | str _ v = toString v
    in
      str 0 v
    end
end

exception ParseError

fun element (TLBrace :: ts) = object ts (Value.Object [])
  | element (TLBracket :: ts) = array ts (Value.Array [])
  | element (TNull :: ts) = (ts, Value.Null)
  | element (TBool b :: ts) = (ts, Value.Bool b)
  | element (TNumber n :: ts) = (ts, Value.Number n)
  | element (TString s :: ts) = (ts, Value.String s)
  | element _ = raise ParseError

and object (TString k :: TColon :: ts) (Value.Object obj) =
    (case (element ts) of
          (TComma :: tr, v) => object tr (Value.Object ((k, v) :: obj))
        | (TRBrace :: tr, v) => (tr, Value.Object ((k, v) :: obj))
        | _ => raise ParseError)
  | object (TRBrace :: ts) obj = (ts, obj)
  | object _ _ = raise ParseError

and array ts (Value.Array a) =
    (case (element ts) of
          (TComma :: tr, e) => array tr (Value.Array (e :: a))
        | (TRBracket :: tr, e) => (tr, Value.Array (e :: a))
        | _ => raise ParseError)
  | array (TRBracket :: ts) a = (ts, a)
  | array _ _ = raise ParseError

fun parse ts =
  case (element ts) of
       ([], v) => v
     | _ => raise ParseError

val v = parse [TLBrace, TString "key", TColon, TNumber 1.0, TRBrace]
val _ = print ((Value.prettyString v) ^ "\n")
