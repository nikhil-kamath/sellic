(* Code for a serial multiplier in Hardcaml *)

open Base

(* The standard bit-shift multiplication algorithm *)

let rec umul a b =
  if b = 0 then 0
  else
    let partial_product = if b land 1 = 1 then a else 0 in
    partial_product + umul (a lsl 1) (b lsr 1)
(* remove the last bit of `b`, shift `a` left 1 *)

let _ =
  assert (umul 2 3 = 6);
  assert (umul 6 0 = 0);
  assert (umul 999 10 = 9990)

(* Hardcaml implementation *)
open Hardcaml
open Hardcaml.Bits

(*
  The output of `a` with width `n` and `b` with width `m` has width `n + m`.
  The following code doesn't work because the carries should have width:
  `width a + width b`, not just `width a`.
*)

let rec umul_h a b =
  if to_int b = 0 then zero (width a)
  else
    let partial_product = mux2 b.:[0, 0] a (zero (width a)) in
    partial_product +: umul_h (sll a 1) (srl b 1)

(* we can fix this by resizing the inputs before passing to the function *)
let umul_h a b = umul_h (uresize a (width a + width b)) b

(*
    Now we can translate this implementation to a hardware description:
*)

open Hardcaml.Signal

let partial_product a b0 = mux2 b0 a (zero (width a))

let running_sum first sum a b0 =
  (* on the first iteration, we should clear the initial sum to 0 *)
  let sum = mux2 first (zero (width sum)) sum in
  ue sum +: ue (partial_product a b0)

let running_sum_reg spec first a b0 =
  let sum_w = wire (width a) -- "running_sum" in
  let running_sum = running_sum first sum_w a b0 -- "running_sum_next" in
  (* split the sum into the least significant bit and the rest *)
  let running_sum_bit_out = lsb running_sum in
  let running_sum = msbs running_sum in
  (* register the sum *)
  let sum = reg spec ~enable:vdd running_sum in
  sum_w <== sum;
  (* `running_sum_bit_out` is the final value of this bit and should be saved to a register.
     The remaining `sum` will be used in further calculations *)
  (sum, running_sum_bit_out)

(* Store the computed bits in a register *)
let computed_bits spec width bit =
  (* Concat-left a new most significant bit to the register *)
  reg_fb spec ~width ~f:(fun d -> bit @: msbs d)

(* Combine everything together *)
let umul_sequential clock first a b_width b0 =
  let spec = Reg_spec.create ~clock () in
  let running_sum, computed_bit = running_sum_reg spec first a b0 in
  let computed_bits = computed_bits spec b_width computed_bit in
  running_sum @: computed_bits

(* Create a circuit *)
let create_circuit a_width b_width =
  let clock = input "clock" 1 in
  let first = input "first" 1 in
  let a = input "a" a_width in
  let b0 = input "b0" 1 in
  let result = umul_sequential clock first a b_width b0 in
  Circuit.create_exn ~name:"umul" [ output "result" result ]

(* We can now simulate the inputs to the circuit and see the waveforms generated *)
module Waveform = Hardcaml_waveterm.Waveform

(* Link up all the circuit's ports with the simulated ones *)
let create_sim circuit =
  let sim = Cyclesim.create ~config:Cyclesim.Config.trace_all circuit in
  let waves, sim = Waveform.create sim in
  let first = Cyclesim.in_port sim "first" in
  let a = Cyclesim.in_port sim "a" in
  let b0 = Cyclesim.in_port sim "b0" in
  let result = Cyclesim.out_port sim "result" in
  (waves, sim, first, a, b0, result)

(* Test the circuit by creating circuits adapted to certain hard-coded inputs (and their widths) *)
let test a_in b_in =
  let open Bits in
  let waves, sim, first, a, b0, result =
    create_circuit (width a_in) (width b_in)
    |> create_sim
  in
  let step iteration =
    first := if iteration = 0 then vdd else gnd;
    b0 := b_in.:[iteration,iteration]; (* get the bit we need *)
    Cyclesim.cycle sim;
  in
  a := a_in;
  for i = 0 to width b_in - 1 do
    step i
  done;
  (* grab the result and perform 1 more cycle so we can see the result in the waveform *)
  let result = !result in
  Cyclesim.cycle sim;
  waves, result

let display_test a_width ~a b_width ~b =
  Stdio.printf "Simulating `%i x %i`\n" a b;
  let waves, result = test (Bits.of_int ~width:a_width a) (Bits.of_int ~width:b_width b) in
  Stdio.printf "Result of circuit simulation: %i\n" (Bits.to_int result);
  Waveform.print ~display_height:25 ~display_width:90 waves

let _ =
  display_test 2 ~a:3 3 ~b:5;
  display_test 7 ~a:100 7 ~b:99;


