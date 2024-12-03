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

let get_arg x =
  match String.lowercase x with
  | "terms" -> `Terms
  | "types" -> `Types
  | "inlined" -> `Inlined
  | "inlinedtypes" -> `InlinedTypes
  | "annotated" -> `Annotated
  | "mults" -> `Mults
  | _ -> `Terms

let command =
  Command.basic ~summary:"Run sellic programs"
    ~readme:(fun () -> "A list of execution options")
    Command.Let_syntax.(
      let%map_open filename = anon ("filename" %: sellic_file)
      and show =
        flag "-show" (listed string)
          ~doc:
            " Pretty print the [ terms | types | inlined | inlinedtypes | \
             mults ]* of a program."
      in
      let whats = List.map ~f:get_arg show in
      fun () ->
        In_channel.with_file filename ~f:(fun file_ic ->
            let lexbuf = Lexing.from_channel file_ic in
            Lp.display_compile ~whats lexbuf))

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command
