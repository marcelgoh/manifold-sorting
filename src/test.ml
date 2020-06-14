(* Tests and plotting running times *)

open Printf
module D = DynArray
module N = Naive_euclidean
module K = Kd_euclidean
module P = Postscript

let run_para_test () =
  let settings = {
    P.scale = 10.0;
    P.colour1 = P.red;
    P.colour2 = P.blue;
    P.xorigin = 20;
    P.yorigin = 20;
    P.draw_circle = 0.0;
  } in
  let plot_one_threshold_naive idx threshold =
    let start_time = Sys.time () in
    let grid = N.fill_para (50.0, -1.0) (5.0, 60.0) threshold (26.5, 24.5) in
    let fill_time = Sys.time () -. start_time in
    let fp = P.create_ps_file (sprintf "test/naive%d" idx) in
    Printf.printf "Grid built in: %fs\n" (Sys.time() -. start_time);
    P.xtick fp settings (-30);
    P.xtick fp settings 30;
    P.plot_grid fp { settings with P.draw_circle = threshold } (N.to_list grid);
    output_string fp (sprintf "30 750 moveto (THRESHOLD: %f) show\n" threshold);
    output_string fp (sprintf "30 735 moveto (NO. POINTS: %d) show\n" (N.grid_size grid));
    P.close_ps_file fp;
    let total_time = Sys.time () -. start_time in
    Printf.printf "Grid plotted in: %fs\n" (total_time -. fill_time);
    Printf.printf "%d points plotted.\n" (N.grid_size grid);
    (N.grid_size grid, fill_time)
  in
  let plot_one_threshold_kd idx threshold =
    let start_time = Sys.time () in
    let grid = K.fill_para (50.0, -1.0) (5.0, 60.0) threshold [26.5; 24.5] in
    let fill_time = Sys.time () -. start_time in
    let fp = P.create_ps_file (sprintf "test/kd%d" idx) in
    Printf.printf "Grid built in: %fs\n" (Sys.time() -. start_time);
    P.xtick fp settings (-30);
    P.xtick fp settings 30;
    P.plot_grid fp { settings with P.draw_circle = threshold } (K.to_list grid);
    output_string fp (sprintf "30 750 moveto (THRESHOLD: %f) show\n" threshold);
    output_string fp (sprintf "30 735 moveto (NO. POINTS: %d) show\n" (K.grid_size grid));
    P.close_ps_file fp;
    let total_time = Sys.time () -. start_time in
    Printf.printf "Grid plotted in: %fs\n" (total_time -. fill_time);
    Printf.printf "%d points plotted.\n" (K.grid_size grid);
    (K.grid_size grid, fill_time)
  in
  let thresholds = [1.21; 1.2; 1.19; 1.18; 1.17; 1.16; 1.15] in
  List.mapi plot_one_threshold_kd thresholds


