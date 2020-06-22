(* Main entry point of the program *)

open Printf

module M =  Metric
module P = Postscript
module T = Test

let _ =
  T.run_para_test "kdonly1" true;
