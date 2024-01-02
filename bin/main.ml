open Reex


module T =
struct
  type span = int * int [@@deriving ord, show]
  type 't parse_tree = Node of 't * span * 't parse_tree list [@@deriving ord, show]
  type tag = Json | Array | Object | Null | False | True | Number | String | Attribute [@@deriving ord, show]
  type t = tag parse_tree [@@deriving ord, show]
end

module Lex =
struct 
open Flap.Cd
  let digit = range '0' '9'
  let nonzero = range '1' '9'
  let hexdigit = digit <|> range 'a' 'f' <|> range 'A' 'F'
  let escape = chr '"' <|> chr '\\' <|> chr '/' <|> chr 'b' <|> chr 'f' <|> chr 'n' <|> chr 'r' <|> chr 't'
  let lbracket = chr '{'
  let rbracket = chr '}'
  let comma = chr ','
  let colon = chr ':'
  let lsquare = chr '['
  let rsquare = chr ']'
  let lit_true = str "true"
  let lit_false = str "false"
  let null = str "null"
  let charset s = Seq.fold_left (fun s c -> s <|> chr c) empty (String.to_seq s)
  let complement s = any <&> not (charset s)
  let string_body = complement "\\\"" <|> (chr '\\' >>> (escape <|> (chr 'u' >>> hexdigit >>> hexdigit >>> hexdigit >>> hexdigit)))
  let string = chr '"' >>> star string_body >>> chr '"'
  let number = opt (chr '-') >>> (chr '0' <|> (nonzero >>> star digit)) >>> opt (chr '.' >>> plus digit) >>> opt (charset "eE" >>> opt (charset "+-") >>> plus digit)
end

open Flap.Parse(T)


let () = print_endline "Hello, World!"


