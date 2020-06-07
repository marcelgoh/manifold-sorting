(* Main entry point of the program *)

open Printf

module M =  Metric
module N = Naive_euclidean
module P = Postscript

let main () =
  let start_time = Sys.time () in
  let grid = N.fill_para (30.0, -10.0) (5.0, 10.0) 1.0 (10.0, 5.0) in
  let fill_time = Sys.time () -. start_time in
  let fp = P.create_ps_file "out/grid" in
  Printf.printf "Grid built in: %fs\n" (Sys.time() -. start_time);
  P.xtick fp P.default (-30);
  P.xtick fp P.default 30;
  P.plot_grid fp P.default grid;
  P.close_ps_file fp;
  let total_time = Sys.time () -. start_time in
  Printf.printf "Grid plotted in: %fs\n" (total_time -. fill_time);
  Printf.printf "%d points plotted.\n" (N.grid_size grid)

let _ = main ()
