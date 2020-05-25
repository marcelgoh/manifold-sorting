(* Main entry point of the program *)

open Printf

module M =  Metric
module N = Naive_euclidean
module P = Postscript

let main () =
  let start_time = Sys.time () in
  let grid = N.fill_rect (-30.0) 30.0 30.0 (-30.0) 0.5 (0.0, 0.0) in
  P.plot_grid grid 5.0 5.0 P.red P.blue "out/grid";
  Printf.printf "Execution time: %fs\n" (Sys.time() -. start_time);
  Printf.printf "%d points plotted.\n" (N.grid_size grid)

let _ = main ()
