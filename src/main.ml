(* Main entry point of the program *)

open Printf

module M =  Metric
module P = Postscript
module T = Test
module H = Halfplane.Halfplane

let _ =
  (* T.run_para_test "kdonly1" true; *)
  let fp = P.create_ps_file "halfplane.ps" in
  P.close_ps_file fp
