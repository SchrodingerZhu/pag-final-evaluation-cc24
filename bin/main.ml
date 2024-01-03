module Parser = struct
  module Lex =
  struct 
    include Reex
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
    let whitespace = plus (charset " \t\r\n")
    
    type t = 
      | Null
      | True
      | False
      | String
      | Number
      | LBracket
      | RBracket
      | LSquare
      | RSquare
      | Comma
      | Colon [@@deriving ord, show]
  end

  module Syntax =
  struct 
    open Flap.Parse(Lex)
    open Flap.Cd

    let lexer = Lex.[
      lsquare,    Return LSquare;
      rsquare,    Return RSquare;
      lbracket,   Return LBracket;
      rbracket,   Return RBracket;
      comma,      Return Comma;
      colon,      Return Colon;
      lit_true,   Return True;
      lit_false,  Return False;
      null,       Return Null;
      number,     Return Number;
      string,     Return String;
      whitespace, Skip;
    ]

    let (/=>) t f = tok t @@ fun s -> injv (f s)
    let comma    = Lex.Comma     /=> fun _ -> .<()>.
    let colon    = Lex.Colon     /=> fun _ -> .<()>.
    let string_   = Lex.String   /=> fun s -> dyn s
    let str_lit   = Lex.String    /=> fun s -> .<`StringLit .~(dyn s)>.
    let lsquare  = Lex.LSquare   /=> fun _ -> .<()>.
    let rsquare  = Lex.RSquare   /=> fun _ -> .<()>.
    let lbracket = Lex.LBracket  /=> fun _ -> .<()>.
    let rbracket = Lex.RBracket  /=> fun _ -> .<()>.
    let number   = Lex.Number    /=> fun s -> .<`Number .~(dyn s)>.
    let null     = Lex.Null      /=> fun _ -> .<`Null>.
    let true_    = Lex.True      /=> fun _ -> .<`True>.
    let false_   = Lex.False     /=> fun _ -> .<`False>.

    let star e = fix @@ fun x -> (eps (injv .<[]>.)
                                    <|> (e >>> x $ fun p -> let_ p @@ fun p ->
                                                            injv .< .~(dyn (fst p)) :: .~(dyn (snd p)) >.))
    let option e = (e $ fun x -> let_ x @@ fun x -> injv .< Option.Some .~(dyn x) >.) <|> (eps (injv .<Option.None>.))
    let parser = (
      fix @@ fun value -> 
        let attr         = string_ >>> colon >>> value $ fun p -> injv .< let ((name,_),v) = .~(dyn p) in (name, v)>. in
        let comma_tail x = comma >>> x $ snd in
        let list x       = x >>> star (comma_tail x) $ fun p -> injv .< let (h, t) = .~(dyn p) in h :: t >. in
        let obj          = lbracket >>> option (list attr) >>> rbracket $ 
                          fun p -> injv .< `Object (match .~(dyn p) with | ((_, None),_) -> [] | ((_, Some x),_) -> x) >. in
        let array        = lsquare >>> option (list value) >>> rsquare $
                          fun p -> injv .< `Array (match .~(dyn p) with | ((_, None),_) -> [] | ((_, Some x),_) -> x) >. in
        str_lit <|> number <|> null <|> true_ <|> false_  <|> obj <|> array
    )
  end
end

open Flap.Parse(Parser.Lex)

let code = Codelib.close_code (Result.get_ok (compile Parser.Syntax.lexer Parser.Syntax.parser))

let parser = Runnative.run_native code

(* json pretty print*)
(* let rec to_string = function
  | `StringLit s -> "\"" ^ s ^ "\""
  | `Number n -> n
  | `Null -> "null"
  | `True -> "true"
  | `False -> "false"
  | `Object [] -> "{}"
  | `Object ((k,v)::xs) -> "{" ^ k ^ ": " ^ (to_string v) ^ (List.fold_left (fun acc (k,v) -> acc ^ ", " ^ k ^ ": " ^ (to_string v)) "" xs) ^ "}"
  | `Array [] -> "[]"
  | `Array (x::xs) -> "[" ^ (to_string x) ^ (List.fold_left (fun acc x -> acc ^ ", " ^ (to_string x)) "" xs) ^ "]" *)
open Core
open Core_bench

let fused_json _ =
  let file = In_channel.read_all "/home/schrodingerzy/Downloads/paguroidea/benches/json/benches/twitter.json" in
  Staged.stage (fun () -> parser file)

let () = 
  Command_unix.run (Bench.make_command [Bench.Test.create_indexed ~name:"fused_json" ~args:[
        0
      ]
      fused_json;])
