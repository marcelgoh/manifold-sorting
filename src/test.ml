(* Tests and plotting running times *)

open Printf
module D = DynArray
module N = Naive_euclidean
module K = Kd_euclidean
module P = Postscript

let run_para_test filename print_output =
  let settings = { P.default with
    P.scale = 10.0;
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
  let rec range a b inc =
    if a > b then [] else a :: range (a +. inc) b inc in
  (* let thresholds = range 0.25 5. 0.25 in *)
  let thresholds = [0.5; 0.505; 0.51; 0.515; 0.52; 0.53; 0.54; 0.55; 0.6; 0.7; 0.8; 1.; 2.; 5.] in
  Printf.printf "kd\n";
  let kd_pts = List.mapi (fun i t -> Printf.printf "%f\n" t; flush stdout; plot_one_threshold_kd i t) thresholds in
  Printf.printf "naive\n";
  let naive_pts = List.mapi (fun i t -> Printf.printf "%f\n" t; flush stdout; plot_one_threshold_naive i t) thresholds in
  let fp = P.create_ps_file ("test/" ^ filename) in
  let xf = 550. /. (List.fold_left max min_float (List.map (fun (_,a,_) -> float_of_int a) kd_pts)) in  (* scale factor *)
  let yf = 700. /. (List.fold_left max min_float (List.map (fun (_,_,a) -> a) kd_pts)) in
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

