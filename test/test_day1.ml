open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Dial = Hardcaml_demo_project.Day1
module Harness = Cyclesim_harness.Make (Dial.I) (Dial.O)

let ( <--. ) = Bits.( <--. )
(* input format *)
type command = 
  | L of int 
  | R of int 
let sample_input_values = [ L 68; L 30; R 48; L 5; R 60; L 55; L 1; L 99; R 14; L 82 ]

let simple_testbench (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  let wait_for_ready () =
    while not (Bits.to_bool !(outputs.ready)) do
      cycle ()
    done
  in
  (* Helper function for inputting one value *)
  let feed_input cmd =
    wait_for_ready ();
    let (value, is_left_bit) = 
      match cmd with
      | L v -> (v, Bits.vdd)
      | R v -> (v, Bits.gnd)
    in
    inputs.magnitude <--. value;
    inputs.is_left := is_left_bit;
    inputs.data_in_valid := Bits.vdd;
    cycle ();
    inputs.data_in_valid := Bits.gnd;
    cycle ()
  in
  (* Reset the design *)
  inputs.clear := Bits.vdd; 
  cycle (); 
  inputs.clear := Bits.gnd; 
  cycle ();
  (* Pulse the start signal *)
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;
  cycle ();
  (* Input some data *)
  List.iter sample_input_values ~f:feed_input;
  (* Finish inputting data *)
  wait_for_ready ();
  inputs.finish := Bits.vdd;
  cycle ();
  inputs.finish := Bits.gnd; 
  cycle ();
  (* Wait for result to become valid *)
  while not (Bits.to_bool !(outputs.zeros_found.valid)) do
    cycle ()
  done;
  let part1 = Bits.to_signed_int !(outputs.zeros_found.value) in
  let part2 = Bits.to_signed_int !(outputs.crossings_count.value) in
  print_s [%message "Results" (part1 : int) (part2 : int)];
  (* Show in the waveform that [valid] stays high. *)
  cycle ~n:2 ()
;;

(* The [waves_config] argument to [Harness.run] determines where and how to save waveforms
   for viewing later with a waveform viewer. The commented examples below show how to save
   a waveterm file or a VCD file. *)
let waves_config = Waves_config.no_waves

(* let waves_config = *)
(*   Waves_config.to_directory "/tmp/" *)
(*   |> Waves_config.as_wavefile_format ~format:Hardcamlwaveform *)
(* ;; *)

(* let waves_config = *)
(*   Waves_config.to_directory "/tmp/" *)
(*   |> Waves_config.as_wavefile_format ~format:Vcd *)
(* ;; *)

let%expect_test "Simple test, optionally saving waveforms to disk" =
  Harness.run_advanced ~waves_config ~create:Dial.hierarchical simple_testbench;
  [%expect {| (Results (part1 3) (part2 6)) |}]
;;

let%expect_test "Simple test with printing waveforms directly" =
  (* For simple tests, we can print the waveforms directly in an expect-test (and use the
     command [dune promote] to update it after the tests run). This is useful for quickly
     visualizing or documenting a simple circuit, but limits the amount of data that can
     be shown. *)
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "day1*" |> Re.compile)
    ]
  in
  Harness.run_advanced
    ~create:Dial.hierarchical
    ~trace:`All_named
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
          (* [display_rules] is optional, if not specified, it will print all named
             signals in the design. *)
        ~signals_width:30
        ~display_width:92
        ~wave_width:1
        (* [wave_width] configures how many chars wide each clock cycle is *)
        waves)
    simple_testbench;
  [%expect
    {|
    (Results (part1 3) (part2 6))
    ┌Signals─────────────────────┐┌Waves───────────────────────────────────────────────────────┐
    │                            ││────────────────────────┬───────────────────┬───────────┬───│
    │day1$crossings_count        ││ 0                      │1                  │2          │3  │
    │                            ││────────────────────────┴───────────────────┴───────────┴───│
    │day1$i$clear                ││────┐                                                       │
    │                            ││    └───────────────────────────────────────────────────────│
    │day1$i$clock                ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                            ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │day1$i$data_in_valid        ││                ┌───┐       ┌───┐   ┌───┐       ┌───┐       │
    │                            ││────────────────┘   └───────┘   └───┘   └───────┘   └───────│
    │day1$i$finish               ││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │day1$i$is_left              ││                ┌───────────────────┐           ┌───────────│
    │                            ││────────────────┘                   └───────────┘           │
    │                            ││────────────────┬───────────┬───────┬───────────┬───────────│
    │day1$i$magnitude            ││ 0              │68         │30     │48         │5          │
    │                            ││────────────────┴───────────┴───────┴───────────┴───────────│
    │day1$i$start                ││        ┌───┐                                               │
    │                            ││────────┘   └───────────────────────────────────────────────│
    │day1$o$crossings_count$valid││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │                            ││────────────────────────────────────────────────────────────│
    │day1$o$crossings_count$value││ 0                                                          │
    │                            ││────────────────────────────────────────────────────────────│
    │day1$o$ready                ││────────────────────┐       ┌───┐   ┌───┐       ┌───┐       │
    │                            ││                    └───────┘   └───┘   └───────┘   └───────│
    │day1$o$zeros_found$valid    ││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │                            ││────────────────────────────────────────────────────────────│
    │day1$o$zeros_found$value    ││ 0                                                          │
    │                            ││────────────────────────────────────────────────────────────│
    │                            ││────────────┬───────┬───┬───────┬───────┬───┬───────┬───┬───│
    │day1$position               ││ 0          │50     │65.│82     │52     │100│0      │65.│95 │
    │                            ││────────────┴───────┴───┴───────┴───────┴───┴───────┴───┴───│
    │                            ││────────────────────────────────────────────────┬───────────│
    │day1$zeros_found            ││ 0                                              │1          │
    │                            ││────────────────────────────────────────────────┴───────────│
    └────────────────────────────┘└────────────────────────────────────────────────────────────┘
    |}]
;;
