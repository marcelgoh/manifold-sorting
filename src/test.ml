(* Tests and plotting running times *)

open Printf
module P = Postscript

let u = (50.0, -1.0)
let v = (5.0, 60.0)
module T = (val Torus.torus u v : Space.Space with type point = float * float)

let pi = (acos (-.1.))

let para_to_euclidean (x, y) =
  let (a, c), (b, d) = u, v in
  let det = (a *. d) -. (c *. b) in
  let a', c', b', d' = d /. det, -.c /. det, -.b /. det, a /. det in
  let x', y' = (a' *. x) +. (b' *. y), (c' *. x) +. (d' *. y) in
  let l = (sqrt ((a ** 2.) +. (b ** 2.))) /. (2. *. pi) in
  [cos (2. *. pi *. x') *. l; cos (2. *. pi *. y') *. l]

module N = Naive.Naive(T)
module K = Kd.Kd(T)(struct let to_e = para_to_euclidean end)

let run_para_test filename print_output =
  let settings = { P.default with
    P.scale = 10.0;
    P.xorigin = 20;
    P.yorigin = 20;
    P.draw_circle = 0.0;
  } in
  let plot_one_grid newfilename threshold to_list grid_size idx grid =
    let thresh_str = Str.global_replace (Str.regexp_string ".") "-" (sprintf "%g" threshold) in
    let fp = P.create_ps_file (sprintf "out/%s--%s" newfilename thresh_str) in
    P.plot_grid fp { settings with P.draw_circle = threshold } (to_list grid);
    output_string fp (sprintf "30 750 moveto (THRESHOLD: %f) show\n" threshold);
    output_string fp (sprintf "30 735 moveto (NO. POINTS: %d) show\n" (grid_size grid));
    P.close_ps_file fp
  in
  let build_naive_pts idx threshold =
    let start_time = Sys.time () in
    let grid = N.build threshold (26.5, 24.5) in
    let fill_time = Sys.time () -. start_time in
    if print_output then plot_one_grid (filename ^ "naive") threshold (fun g -> List.map T.to_screen (N.to_list g)) N.grid_size idx grid;
    (threshold, N.grid_size grid, fill_time)
  in
  let build_kd_pts idx threshold =
    let start_time = Sys.time () in
    let grid = K.build threshold (26.5, 24.5) in
    let fill_time = Sys.time () -. start_time in
    if print_output then plot_one_grid (filename ^ "kd") threshold (fun g -> List.map T.to_screen (K.to_list g)) K.grid_size idx grid;
    (threshold, K.grid_size grid, fill_time)
  in
  let max_in_list_float = List.fold_left max min_float in
  let max_in_list_int = List.fold_left max min_int in
  let thresholds = [0.27; 0.28; 0.29; 0.30; 0.31; 0.315; 0.32; 0.325; 0.33; 0.34; 0.36; 0.38; 0.40; 0.42; 0.44; 0.46; 0.48; 0.5; 0.52; 0.54; 0.55; 0.57; 0.6; 0.7; 0.8; 1.; 2.; 3.; 4.; 5.] in
(* let thresholds = [0.5; 0.515; 0.52; 0.53; 0.54; 0.55; 0.6; 0.62; 0.65; 0.7; 0.75; 0.8; 0.9; 1.; 1.1; 1.2; 1.3; 1.4; 1.5; 2.; 3.; 4.; 5.] in *)
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
(*
    P.xmax = max_in_list_int (List.map second_triple naive_pts);
    P.ymax = max_in_list_float (List.map third_triple naive_pts);
*)
    P.xmax = max_in_list_int (List.map second_triple kd_pts);
    P.ymax = max_in_list_float (List.map third_triple kd_pts);
    P.ylabeloffsets = 22;
    P.write_thresholds = true;
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
(*   P.draw_graph fp graph_settings [(P.green, kd_pts); (P.red, naive_pts)]; *)
  P.draw_graph fp graph_settings [(P.green, kd_pts)];
  P.close_ps_file fp

