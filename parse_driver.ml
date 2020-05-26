open Lexing
open Printf

module I = Parser.MenhirInterpreter

exception Syntax_error of int * int * string
exception Error

let get_parse_error env =
    match I.stack env with
    | lazy Nil -> "Invalid syntax"
    | lazy (Cons (I.Element (state, _, _, _), _)) ->
        try (
            Parser_messages.message (I.number state)
        ) with
        | Not_found -> "invalid syntax (no specific message for this error)"

let rec parse lexbuf (checkpoint : Ast.program I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env ->
      let token = Scanner.token lexbuf in
      let startp = lexbuf.lex_start_p
      and endp = lexbuf.lex_curr_p in
      let checkpoint = I.offer checkpoint (token, startp, endp) in
      parse lexbuf checkpoint
  | I.Shifting _
  | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      parse lexbuf checkpoint
  | I.HandlingError _env ->
      let pos = Lexing.lexeme_start_p lexbuf in
      let line = pos.Lexing.pos_lnum in
      let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
      let err = get_parse_error _env in
      raise (Syntax_error (line, col, err))
  | I.Accepted v -> v
  | I.Rejected ->
       raise (Syntax_error (-1, -1, "invalid syntax (parser rejected the input)"))


let parse_program filename =
    let lexbuf = Lexing.from_channel (open_in filename) in
    try 
        parse lexbuf (Parser.Incremental.program lexbuf.lex_curr_p)
    with
        Syntax_error(line, col, msg) -> 
        Printf.printf "%s:%d:%d: Syntax Error: %s" filename line col msg; raise Error

