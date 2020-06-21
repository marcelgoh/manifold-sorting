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
  let plot_one_grid newfilename threshold to_list grid_size idx grid =
    let fp = P.create_ps_file (sprintf "out/%s--%d" newfilename idx) in
    P.plot_grid fp { settings with P.draw_circle = threshold } (to_list grid);
    output_string fp (sprintf "30 750 moveto (THRESHOLD: %f) show\n" threshold);
    output_string fp (sprintf "30 735 moveto (NO. POINTS: %d) show\n" (grid_size grid));
    P.close_ps_file fp
  in
  let build_naive_pts idx threshold =
    let start_time = Sys.time () in
    let grid = N.fill_para (50.0, -1.0) (5.0, 60.0) threshold (26.5, 24.5) in
    let fill_time = Sys.time () -. start_time in
    if print_output then plot_one_grid (filename ^ "naive") threshold N.to_list N.grid_size idx grid;
    (threshold, N.grid_size grid, fill_time)
  in
  let build_kd_pts idx threshold =
    let start_time = Sys.time () in
    let grid = K.fill_para (50.0, -1.0) (5.0, 60.0) threshold [26.5; 24.5] in
    let fill_time = Sys.time () -. start_time in
    if print_output then plot_one_grid (filename ^ "kd") threshold K.to_list K.grid_size idx grid;
    (threshold, K.grid_size grid, fill_time)
  in
  let max_in_list_float = List.fold_left max min_float in
  let max_in_list_int = List.fold_left max min_int in
  let thresholds = [0.5; 0.505; 0.51; 0.515; 0.52; 0.53; 0.54; 0.55; 0.6; 0.7; 0.8; 1.; 2.; 5.] in
  let fp = P.create_ps_file ("test/" ^ filename) in
  Printf.printf "kd\n";
  let kd_pts = List.mapi (fun i t -> Printf.printf "%f\n" t; flush stdout; build_kd_pts i t) thresholds in
(*
  Printf.printf "naive\n";
  let naive_pts = List.mapi (fun i t -> Printf.printf "%f\n" t; flush stdout; build_naive_pts i t) thresholds in
*)
  let first_triple (a, _, _) = a in
  let second_triple (_, b, _) = b in
  let third_triple (_, _, c) = c in
  let graph_settings = { P.default_graph with
    P.xmax = max_in_list_int (List.map second_triple kd_pts);
    P.ymax = max_in_list_float (List.map third_triple kd_pts);
  } in
  let print_info () kd_pts naive_pts =
    P.moveto fp 30 750;
    P.show_str fp "THRESHOLD: ";
    List.iter (P.show_float fp) (List.map first_triple kd_pts);
    P.moveto fp 30 735;
    P.show_str fp "KD_PTS:    ";
    List.iter (P.show_int fp) (List.map second_triple kd_pts);
    P.moveto fp 30 720;
    P.show_str fp "NAIVE_PTS: ";
    List.iter (P.show_int fp) (List.map second_triple naive_pts);
    P.moveto fp 30 705;
    P.show_str fp "KD_TIME:   ";
    List.iter (P.show_float fp) (List.map third_triple kd_pts);
    P.moveto fp 30 690;
    P.show_str fp "NAIVE_TIME:";
    List.iter (P.show_float fp) (List.map third_triple naive_pts);
  in
  P. draw_graph fp graph_settings [(P.green, kd_pts)];
  P.close_ps_file fp

