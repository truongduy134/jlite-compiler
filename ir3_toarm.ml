(* ===================================================== *)
(* ============== CS4212 Compiler Design ============== *)
(*   Transformation from IR3 to ARM assembly code   *)
(* ===================================================== *)

open Arm_structs
open Ir3_structs

(* Transform an IR3 program to IR3 *)
let ir3_program_to_arm
  ((struct_list, main_md, md_list): ir3_program): arm_program = []
