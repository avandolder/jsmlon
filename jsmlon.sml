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

structure JSValue = struct
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

structure Scan = struct
  fun scan (#"[" :: cs) ts = scan cs (TLBracket :: ts)
    | scan (#"]" :: cs) ts = scan cs (TRBracket :: ts)
    | scan (#"{" :: cs) ts = scan cs (TLBrace :: ts)
    | scan (#"}" :: cs) ts = scan cs (TRBrace :: ts)
    | scan (#":" :: cs) ts = scan cs (TColon :: ts)
    | scan (#"," :: cs) ts = scan cs (TComma :: ts)
    | scan (#"\"" :: cs) ts = scanString [] cs ts
    | scan (#"-" :: cs) ts = scanNumber (#"-" :: cs) ts
    | scan (c :: cs) ts =
        if Char.isAlpha c then scanName (c :: cs) ts
        else if Char.isDigit c then scanNumber (c :: cs) ts
        else if Char.isSpace c then scan cs ts
        else raise ParseError
    | scan [] ts = ts

  and scanString s (#"\"" :: cs) ts =
        scan cs ((TString (String.implode (List.rev s))) :: ts)
    | scanString s (#"\\" :: cs) ts =
        (case cs of
            #"\"" :: cs => scanString (#"\"" :: s) cs ts
          | #"t" :: cs => scanString (#"\t" :: s) cs ts
          | #"\\" :: cs => scanString (#"\\" :: s) cs ts
          | #"/" :: cs => scanString (#"/" :: s) cs ts
          | #"n" :: cs => scanString (#"\n" :: s) cs ts
          | #"r" :: cs => scanString (#"\r" :: s) cs ts
          | #"b" :: cs => scanString (#"\b" :: s) cs ts
          | #"f" :: cs => scanString (#"\f" :: s) cs ts
          | _ => raise ParseError)
    | scanString s (c :: cs) ts = scanString (c :: s) cs ts
    | scanString _ _ _ = raise ParseError

  and scanNumber cs ts =
    let
      fun toNum n =
            (case Real.fromString (String.implode (List.rev n)) of
                SOME n => TNumber n
              | NONE => raise ParseError)

      fun firstDigit n (#"0" :: cs) = frac (#"0" :: n) cs
        | firstDigit n (d :: cs) =
            if Char.isDigit d then digits (d :: n) cs
            else raise ParseError
        | firstDigit n [] = raise ParseError

      and digits n (d :: cs) =
            if Char.isDigit d then digits (d :: n) cs
            else frac n (d :: cs)
        | digits n [] = (toNum n) :: ts
     
      and frac n (#"." :: cs) = fracDigits (#"." :: n) cs
        | frac n cs = expo n cs

      and fracDigits n (d :: cs) =
            if Char.isDigit d then fracDigits (d :: n) cs
            else expo n (d :: cs)
        | fracDigits n [] = (toNum n) :: ts

      and expo n (#"e" :: cs) = expoSign (#"e" :: n) cs
        | expo n (#"E" :: cs) = expoSign (#"E" :: n) cs
        | expo n cs = scan cs ((toNum n) :: ts)

      and expoSign n (#"+" :: cs) = expoDigits (#"+" :: n) cs
        | expoSign n (#"-" :: cs) = expoDigits (#"-" :: n) cs
        | expoSign n (d :: cs) =
            if Char.isDigit d then expoDigits (d :: n) cs
            else raise ParseError
        | expoSign n [] = raise ParseError

      and expoDigits n (d :: cs) =
            if Char.isDigit d then expoDigits (d :: n) cs
            else scan cs ((toNum n) :: ts)
        | expoDigits n [] = (toNum n) :: ts
    in
      case cs of
          #"-" :: cs => firstDigit [#"~"] cs
        | _ => firstDigit [] cs
    end

  and scanName cs ts =
    let
      val toName = String.implode o List.rev
      fun token "null" = TNull
        | token "true" = TBool true
        | token "false" = TBool false
        | token _ = raise ParseError
      fun scanChar name (c :: cs) =
            if Char.isAlpha c then scanChar (c :: name) cs
            else scan cs ((token (toName name)) :: ts)
        | scanChar name [] = (token (toName name)) :: ts
    in
      scanChar [] cs
    end
end

fun scan src = List.rev (Scan.scan (String.explode src) [])

structure Parse = struct
  fun element (TLBrace :: ts) = object ts (JSValue.Object [])
    | element (TLBracket :: ts) = array ts (JSValue.Array [])
    | element (TNull :: ts) = (ts, JSValue.Null)
    | element (TBool b :: ts) = (ts, JSValue.Bool b)
    | element (TNumber n :: ts) = (ts, JSValue.Number n)
    | element (TString s :: ts) = (ts, JSValue.String s)
    | element _ = raise ParseError

  and object (TString k :: TColon :: ts) (JSValue.Object obj) =
      (case (element ts) of
            (TComma :: tr, v) => object tr (JSValue.Object ((k, v) :: obj))
          | (TRBrace :: tr, v) => (tr, JSValue.Object ((k, v) :: obj))
          | _ => raise ParseError)
    | object (TRBrace :: ts) obj = (ts, obj)
    | object _ _ = raise ParseError

  and array ts (JSValue.Array a) =
      (case (element ts) of
            (TComma :: tr, e) => array tr (JSValue.Array (e :: a))
          | (TRBracket :: tr, e) => (tr, JSValue.Array (e :: a))
          | _ => raise ParseError)
    | array (TRBracket :: ts) a = (ts, a)
    | array _ _ = raise ParseError
end

fun parse ts =
  case (Parse.element ts) of
       ([], v) => v
     | _ => raise ParseError

val v = parse [TLBrace, TString "key", TColon, TNumber 0.01, TRBrace]
val () = print ((JSValue.prettyString v) ^ "\n")

val tokens = scan (JSValue.toString v)
val value = parse tokens
val () = print ((JSValue.toString value) ^ "\n")
