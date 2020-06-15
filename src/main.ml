(* Main entry point of the program *)

open Printf

module M =  Metric
module N = Naive_euclidean
module K = Kd_euclidean
module P = Postscript
module T = Test

let naive_main () =
  let start_time = Sys.time () in
  let grid = N.fill_para (10.0, -3.0) (-2.0, 15.0) 0.5 (1.0, 1.0) in
  let fill_time = Sys.time () -. start_time in
  let fp = P.create_ps_file "out/naive" in
  Printf.printf "Grid built in: %fs\n" (Sys.time() -. start_time);
  P.xtick fp P.default (-30);
  P.xtick fp P.default 30;
  P.plot_grid fp P.default (N.to_list grid);
  P.close_ps_file fp;
  let total_time = Sys.time () -. start_time in
  Printf.printf "Grid plotted in: %fs\n" (total_time -. fill_time);
  Printf.printf "%d points plotted.\n" (N.grid_size grid)

let kd_main () =
  let start_time = Sys.time () in
  let grid = K.fill_rect (-30.0) 30.0 30.0 (-30.0) 0.5 [0.0; 0.0] in
  let fill_time = Sys.time () -. start_time in
  let fp = P.create_ps_file "out/kd" in
  Printf.printf "Grid built in: %fs\n" (Sys.time() -. start_time);
  P.xtick fp P.default (-30);
  P.xtick fp P.default 30;
  P.plot_grid fp P.default (K.to_list grid);
  P.close_ps_file fp;
  let total_time = Sys.time () -. start_time in
  Printf.printf "Grid plotted in: %fs\n" (total_time -. fill_time);
  Printf.printf "%d points plotted.\n" (K.grid_size grid)

let _ =
  T.run_para_test 1.0 "timegraph1-0" true;
  T.run_para_test 3.0 "timegraph3-0" true;
  T.run_para_test 5.0 "timegraph5-0" true
