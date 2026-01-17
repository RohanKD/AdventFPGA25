(* An example design that takes a series of input values and calculates the range between
   the largest and smallest one. *)

(* We generally open Core and Hardcaml in any source file in a hardware project. For
   design source files specifically, we also open Signal. *)
open! Core
open! Hardcaml
open! Signal

let num_bits = 16

(* Every hardcaml module should have an I and an O record, which define the module
   interface. *)
module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; magnitude : 'a [@bits num_bits] (* Unsigned input value *)
    ; is_left : 'a                    (* 1 = Left (-), 0 = Right (+) *)
    ; data_in_valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { (* With_valid.t is an Interface type that contains a [valid] and a [value] field. *)
      zeros_found : 'a With_valid.t [@bits num_bits]
    ; crossings_count : 'a With_valid.t [@bits num_bits]
    ; ready : 'a
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Waiting_for_input
    | Normalizing
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({ clock; clear; start; finish; magnitude; is_left; data_in_valid } : _ I.t) : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm = 
    (* Note that the state machine defaults to initializing to the first state *)
    State_machine.create (module States) spec in
  (* let%hw[_var] is a shorthand that automatically applies a name to the signal, which
     will show up in waveforms. The [_var] version is used when working with the Always
     DSL. *)
  let%hw_var position = Variable.reg spec ~width:num_bits in
  let%hw_var zeros_found = Variable.reg spec ~width:num_bits in
  let%hw_var crossings_count = Variable.reg spec ~width:num_bits in
  let%hw_var did_overflow = Variable.reg spec ~width:1 in
  (* We don't need to name the range here since it's immediately used in the module
     output, which is automatically named when instantiating with [hierarchical] *)
  let zeros_out = Variable.wire ~default:(zero num_bits) () in
  let crossings_out = Variable.wire ~default:(zero num_bits) () in
  let result_valid = Variable.wire ~default:gnd () in
  let ready_out = Variable.wire ~default:gnd () in
  (*constants for arithmetic *)
  let c_100 = of_int_trunc ~width:num_bits 100 in
  let c_50  = of_int_trunc ~width:num_bits 50 in
  let c_1   = of_int_trunc ~width:num_bits 1 in
  compile
    [ sm.switch
        [ ( Idle
          , [ ready_out <-- vdd
            ; when_
                start
                [ position <-- c_50 (*init position to 50*)
                ; zeros_found <-- zero num_bits
                ; crossings_count <-- zero num_bits
                ; did_overflow <-- gnd
                ; sm.set_next Waiting_for_input
                ]
            ] )
        ; ( Waiting_for_input
          , [ ready_out <-- vdd
            ; when_
                data_in_valid
                [ (* Checks direction to add or subtract *)
                  if_ is_left
                    [ position <-- position.value -: magnitude ]
                  @@ else_
                    [ position <-- position.value +: magnitude ]
                ; did_overflow <-- gnd
                ; sm.set_next Normalizing
                ]
            ; when_ finish [ sm.set_next Done ]
            ] )
        ; ( Normalizing
          , [ (* Repeatedly adds or subtracts 100 til position is in [0,99] *)
              if_ (msb position.value)
                [ position <-- position.value +: c_100 
                (* if we have to normalize by 100 it means we crossed 0*)
                ; crossings_count <-- crossings_count.value +: c_1 
                ; did_overflow <-- vdd 
                ]
              @@ elif (position.value >=: c_100)
                   [ position <-- position.value -: c_100 
                   (* if we have to normalize by 100 it means we crossed 0*)
                   ; crossings_count <-- crossings_count.value +: c_1
                   ; did_overflow <-- vdd 
                   ]
              @@ else_
                   [ when_ (position.value ==: zero num_bits) 
                   (* if we land on 0 we can increment the part 1 answer*)
                       [ zeros_found <-- zeros_found.value +: c_1 ]
                   ; sm.set_next Waiting_for_input
                   ]
            ] )
        ; ( Done
          , [ zeros_out <-- zeros_found.value
            ; crossings_out <-- crossings_count.value
            ; result_valid <-- vdd
            ; when_ finish [ sm.set_next Waiting_for_input ]
            ] )
        ]
    ];
  (* [.value] is used to get the underlying Signal.t from a Variable.t in the Always DSL. *)
  { zeros_found = { value = zeros_out.value; valid = result_valid.value } 
  ; crossings_count = { value = crossings_out.value; valid = result_valid.value } 
  ; ready = ready_out.value 
  }
;;

(* The [hierarchical] wrapper is used to maintain module hierarchy in the generated
   waveforms and (optionally) the generated RTL. *)
let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day1" create
;;