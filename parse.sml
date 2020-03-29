datatype terminal =
      LBracket
    | RBracket
    | LBrace
    | RBrace
    | Colon
    | Comma
    | Null
    | Boolean of bool
    | Number of real
    | String of string

exception ParseError

fun element ts =
  case ts of
       LBrace :: tr => object tr
     | LBracket :: tr => array tr
     | Null :: tr => tr
     | Boolean b :: tr => tr
     | Number n :: tr => tr
     | String s :: tr => tr
     | _ => raise ParseError
and object ts =
  case ts of
       (String key) :: Colon :: tr =>
         (case (element tr) of
               Comma :: tt => object tt
             | RBrace :: tt => tt
             | _ => raise ParseError)
     | RBrace ::tr => tr
     | _ => raise ParseError
and array ts =
  case ts of
       RBracket :: tr => tr
     | _ => let val tr = element ts
            in (case tr of
                     Comma :: tt => array tt
                   | RBracket :: tt => tt
                   | _ => raise ParseError)
            end

fun parse ts =
  case (element ts) of
       [] => ()
     | _ => raise ParseError

val () = parse [LBrace, String "key", Colon, Number 1.0, RBrace]
