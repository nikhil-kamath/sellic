open Core
open Play

let ( = ) = Poly.( = )

let get_file_extension filename =
  String.split_on_chars filename ~on:[ '.' ]
  |> List.last |> Option.value ~default:""

let rec remove_last_elem_list = function
  | [] -> []
  | [ _ ] -> []
  | x :: xs -> x :: remove_last_elem_list xs

let get_output_file filename =
  String.split_on_chars filename ~on:[ '.' ] |> fun split_filename ->
  remove_last_elem_list split_filename |> fun filename_without_ending ->
  String.concat ~sep:"." (filename_without_ending @ [ "ir" ])

let sellic_file =
  let error_not_file filename =
    eprintf "'%s' is not a sellic file. Hint: use the .sl extension\n%!"
      filename;
    exit 1
  in
  Command.Spec.Arg_type.create (fun filename ->
      match Sys_unix.is_file filename with
      | `Yes ->
          if get_file_extension filename = "sl" then filename
          else error_not_file filename
      | `No | `Unknown -> error_not_file filename)

let command =
  Command.basic ~summary:"Run sellic programs"
    ~readme:(fun () -> "A list of execution options")
    Command.Let_syntax.(
      let%map_open filename = anon ("filename" %: sellic_file)
      and show_typed_ast =
        flag "-show-typed-ast" no_arg
          ~doc:" Pretty print the typed AST of the program"
      in
      fun () ->
        In_channel.with_file filename ~f:(fun file_ic ->
            let lexbuf =
              Lexing.from_channel file_ic
              (*Create a lex buffer from the file to read in tokens *)
            in
            Lp.display_compile ~show_typed_ast lexbuf))

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command
