open Unix

let python_script_path = "/Users/nikhil/Code/sellic/play/py/predict_cli.py"

(* Helper function to run a command and capture the output *)
let run_command cmd =
  let in_channel = Unix.open_process_in cmd in
  let output = input_line in_channel in
  ignore (Unix.close_process_in in_channel);
  output

let predict_mult arg1 arg2 arg3 =
  let cmd = Printf.sprintf "%s predict_mult %f %f %f" python_script_path arg1 arg2 arg3 in
  int_of_string (run_command cmd)

let predict_sparsity arg1 arg2 arg3 =
  let cmd = Printf.sprintf "%s predict_sparsity %f %f %f" python_script_path arg1 arg2 arg3 in
  float_of_string (run_command cmd)

let predict_rlv arg1 arg2 arg3 =
  let cmd = Printf.sprintf "%s predict_rlv %f %f %f" python_script_path arg1 arg2 arg3 in
  float_of_string (run_command cmd)
