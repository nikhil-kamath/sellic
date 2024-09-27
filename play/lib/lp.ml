open Core
open Lexer
open Lexing

(* Prints the line number and character number where the error occurred.*)
let print_error_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  sprintf "Line:%d Position:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_program lexbuf =
  try Ok (Parser.program Lexer.read_token lexbuf) with
  (* Unfortunately the lexer and parser throw exceptions - so here we swallow the exn into
     the Result monad*)
  | SyntaxError msg ->
      let error_msg = sprintf "%s: %s@." (print_error_position lexbuf) msg in
      Error (Error.of_string error_msg)
  | Parser.Error ->
      let error_msg =
        sprintf "%s: syntax error@." (print_error_position lexbuf)
      in
      Error (Error.of_string error_msg)

let show_parsed_file lexbuf =
  match parse_program lexbuf with
  | Ok p -> Term.show_program p
  | Error e -> Error.to_string_mach e
