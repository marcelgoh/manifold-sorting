(* Main entry point of the program *)

open Printf

module M =  Metric
module N = Naive_euclidean
module P = Postscript

let main () =
  let start_time = Sys.time () in
  let grid = N.fill_para (30.0, -10.0) (5.0, 10.0) 1.0 (10.0, 5.0) in
  let fill_time = Sys.time () -. start_time in
  Printf.printf "Grid built in: %fs\n" (Sys.time() -. start_time);
  P.plot_grid grid 7.0 7.0 P.red P.blue "out/grid";
  let total_time = Sys.time () -. start_time in
  Printf.printf "Grid plotted in: %fs\n" (total_time -. fill_time);
  Printf.printf "%d points plotted.\n" (N.grid_size grid)

let _ = main ()
