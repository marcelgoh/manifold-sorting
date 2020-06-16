(* Tests and plotting running times *)

open Printf
module D = DynArray
module N = Naive_euclidean
module K = Kd_euclidean
module P = Postscript

let run_para_test filename print_output =
  let settings = {
    P.scale = 10.0;
    P.colour1 = P.red;
    P.colour2 = P.blue;
    P.xorigin = 20;
    P.yorigin = 20;
    P.draw_circle = 0.0;
  } in
  let plot newfilename threshold to_list grid_size idx grid =
    let fp = P.create_ps_file (sprintf "out/%s--%d" newfilename idx) in
    P.plot_grid fp { settings with P.draw_circle = threshold } (to_list grid);
    output_string fp (sprintf "30 750 moveto (THRESHOLD: %f) show\n" threshold);
    output_string fp (sprintf "30 735 moveto (NO. POINTS: %d) show\n" (grid_size grid));
    P.close_ps_file fp
  in
  let plot_one_threshold_naive idx threshold =
    let start_time = Sys.time () in
    let grid = N.fill_para (50.0, -1.0) (5.0, 60.0) threshold (26.5, 24.5) in
    let fill_time = Sys.time () -. start_time in
    if print_output then plot (filename ^ "naive") threshold N.to_list N.grid_size idx grid;
    (threshold, N.grid_size grid, fill_time)
  in
  let plot_one_threshold_kd idx threshold =
    let start_time = Sys.time () in
    let grid = K.fill_para (50.0, -1.0) (5.0, 60.0) threshold [26.5; 24.5] in
    let fill_time = Sys.time () -. start_time in
    if print_output then plot (filename ^ "kd") threshold K.to_list K.grid_size idx grid;
    (threshold, K.grid_size grid, fill_time)
  in
  let thresholds = [1.1; 1.12; 1.15; 1.2; 1.25; 1.3; 1.4; 1.5; 1.6; 1.7; 2.0; 3.0; 4.0; 5.0] in
  let kd_pts = List.mapi plot_one_threshold_kd thresholds in
  let naive_pts = List.mapi plot_one_threshold_naive thresholds in
  let fp = P.create_ps_file ("test/" ^ filename) in
  let xf = 0.3 in  (* scale factor *)
  let yf = 70.0 in
  let plot (_, x',y) =
    let x = float_of_int x' in
    output_string fp (sprintf "%f %f dot\n" (x *. xf) (y *. yf)) in
  let show_float flt = output_string fp (sprintf "(%g ) show\n" flt) in
  let show_int n = output_string fp (sprintf "(%d ) show\n" n) in
  let moveto x y str = output_string fp (sprintf "%d %d moveto (%s ) show\n" x y str) in
  P.set_colour fp P.green;
  List.iter plot kd_pts;
  P.set_colour fp P.red;
  List.iter plot naive_pts;
  P.set_colour fp P.black;
  moveto 30 750 "THRESHOLD: ";
  List.iter (fun (f, _, _) -> show_float f) kd_pts;
  moveto 30 735 "KD_PTS:    ";
  List.iter (fun (_, n, _) -> show_int n) kd_pts;
  moveto 30 720 "NAIVE_PTS: ";
  List.iter (fun (_, n, _) -> show_int n) naive_pts;
  moveto 30 705 "KD_TIME:   ";
  List.iter (fun (_, _, f) -> show_float f) kd_pts;
  moveto 30 690 "NAIVE_TIME:";
  List.iter (fun (_, _, f) -> show_float f) naive_pts;
  P.close_ps_file fp

