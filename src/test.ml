(* Tests and plotting running times *)

open Printf
module P = Postscript

let u = (50.0, -1.0)
let v = (5.0, 60.0)
module T = (val Torus.torus u v : Space.Space with type point = float * float)
module H = Halfplane.Halfplane
module E = Euclidean.Euclidean

let pi = (acos (-.1.))

let tail_mapi f x_list =
  let rec helper idx acc x_list =
    match x_list with
    | [] -> List.rev acc
    | x :: xs -> helper (idx + 1) ((f idx x) :: acc) xs
  in
  helper 0 [] x_list

let para_to_euclidean (x, y) =
  let (a, c), (b, d) = u, v in
  let det = (a *. d) -. (c *. b) in
  let a', c', b', d' = d /. det, -.c /. det, -.b /. det, a /. det in
  let x', y' = (a' *. x) +. (b' *. y), (c' *. x) +. (d' *. y) in
  let l = (sqrt ((a ** 2.) +. (b ** 2.))) /. (2. *. pi) in
  [cos (2. *. pi *. x') *. l; cos (2. *. pi *. y') *. l]

module N = Naive.Naive(T)
module K = Kd.Kd(T)(struct let to_e p t = let p' = para_to_euclidean p in (p', List.map (fun x -> (x -. t, x +. t)) p') end)
module Ke = Kd.Kd(E)(struct let to_e (x, y) t = ([x; y], [(x -. t, x +. t); (y -. t, y +. t)]) end)
module Ke2 = Kd.Kd(E)(struct let to_e (x, y) t =
                              let r = E.dist (x, y) (0., 1.) in
                              let theta = atan2 y x in
                              let rect = [(r -. t, r +. t);
                                          (if r <= t then (-4., 4.) else
                                             let d = t /. (r -. t) in
                                             (theta -. d, theta +. d)
                                         )]
                              in
                              ([r; theta], rect) end)

module Nh = Naive.Naive(H)
module Kh = Kd.Kd(H)(struct let to_e (x, y) threshold =
                                 let r = H.dist (x, y) (0., 1.) in
                                 let theta = atan2 (x ** 2. +. y ** 2. -. 1.) (2. *. x) in
                                 let rect = [(r -. threshold, r +. threshold);
                                             (if r <= threshold then (-4., 4.) else
                                                let d = threshold /. (sinh (r -. threshold)) in
                                                ((cos theta) -. d, (cos theta) +. d)
                                             );
                                             (if r <= threshold then (-4., 4.) else
                                                let d = threshold /. (sinh (r -. threshold)) in
                                                ((sin theta) -. d, (sin theta) +. d)
                                            )]
                                 in
                                 [r; cos theta; sin theta], rect end)

let first_triple (a, _, _) = a
let second_triple (_, b, _) = b
let third_triple (_, _, c) = c
let max_in_list_float = List.fold_left max min_float
let max_in_list_int = List.fold_left max min_int
let plot_one_grid newfilename radius scale to_list grid_size idx grid =
  let settings = { P.default with
    P.xorigin = 306;
    P.yorigin = 20;
  } in
  let fp = P.create_ps_file newfilename in
  P.plot_grid fp { settings with scale=scale } (to_list grid);
  output_string fp (sprintf "30 750 moveto (RADIUS:     %f) show\n" radius);
  output_string fp (sprintf "30 735 moveto (NO. POINTS: %d) show\n" (grid_size grid));
  P.close_ps_file fp

let halfplane_comp_test filename ball_radius =
  let make_triple i (c, added) = (added, (float_of_int (i+1), float_of_int c)) in
(*
  let (_, naive_comp_counts) = Nh.fill_ball (0.0, 1.0) ball_radius 0.5 (0.0, 1.0) in
  let naive_pts = tail_mapi make_triple naive_comp_counts in
 *)
(*   let (_, kd_comp_counts) = Ke.fill_ball (0., 0.) 70.0 0.6 (0.0, 0.0) in *)
  (*   let (_, kd_comp_counts) = K.fill_space 0.4 (27.5, 29.5) in *)
  let (grid, kd_comp_counts) = Kh.fill_ball (0., 1.) ball_radius 0.5 (0., 1.) in
  let kd_pts = tail_mapi make_triple kd_comp_counts in
  let added_pts = List.map snd (List.filter fst kd_pts) in
  let excluded_pts = List.map snd (List.filter (fun x -> not (fst x)) kd_pts) in
  let graph_settings = { P.default_graph with
    P.xmax = max_in_list_float (List.map (fun x -> fst (snd x)) kd_pts);
(*     P.ymax = max_in_list_float (List.map third_triple naive_pts); *)
    P.ymax = max_in_list_float (List.map (fun x -> snd (snd x)) kd_pts);
    P.ylabeloffsets = 22;
    P.write_thresholds = false;
  } in
  (* Printf.printf "KD:\n";
   * List.iter (fun (_, (i, c)) -> Printf.printf "%f %f\n" i c) kd_pts; *)
(*
  Printf.printf "NAIVE:\n";
  List.iter (fun (_, i, c) -> Printf.printf "%d %f\n" i c) naive_pts;
*)
  let fp = P.create_ps_file ("test/" ^ filename) in
(*   P.draw_graph fp graph_settings [(P.green, kd_pts); (P.red, naive_pts)] "POINT" ""; *)
  P.scatterplot fp graph_settings [(P.green, added_pts); (P.red, excluded_pts)];
  P.close_ps_file fp;
  Printf.printf "%d\n" (Kh.grid_size grid); ()
  (* plot_one_grid "problempoints" ball_radius (300.0 /. (sinh ball_radius))
   *      (fun g -> List.map (fun p -> let (x, y) = p in Printf.printf "(%f, %f)\n%f, %f, %f\n%f\n\n" (fst p) (snd p) (H.dist p (0., 1.)) (atan2 (x ** 2. +. y ** 2. -. 1.) (2. *. x)) (cos (atan2 (x ** 2. +. y ** 2. -. 1.) (2. *. x))) (sqrt (x ** 2. +. y ** 2.)); H.to_screen p 0.5) (Kh.to_list g)) Kh.grid_size 0 grid *)

let run_halfplane_test filename print_output =
  let build_naive_pts idx ball_radius =
    let start_time = Sys.time () in
    let (grid, _) = Nh.fill_ball (0.0, 1.0) ball_radius 0.5 (0.0, 1.0) in
    let fill_time = Sys.time () -. start_time in
    if print_output then
      plot_one_grid (filename ^ "naive") ball_radius (300.0 /. (sinh ball_radius))
        (fun g -> List.map (fun p -> H.to_screen p 0.5) (Nh.to_list g)) Nh.grid_size idx grid;
    (ball_radius, Nh.grid_size grid, fill_time)
  in
  let build_kd_pts idx ball_radius =
    let start_time = Sys.time () in
    let (grid, _) = Kh.fill_ball (0.0, 1.0) ball_radius 0.5 (0.0, 1.0) in
    let fill_time = Sys.time () -. start_time in
    Printf.printf "%d %f\n" (Kh.grid_size grid) fill_time;
    if print_output then (
      let radius_str = Str.global_replace (Str.regexp_string ".") "-" (sprintf "%g" ball_radius) in
      let newfilename = (sprintf "out/%s--%s" filename radius_str) in
      plot_one_grid (newfilename ^ "kd") ball_radius (300.0 /. (sinh ball_radius))
          (fun g -> List.map (fun p -> H.to_screen p 0.5) (Kh.to_list g)) Kh.grid_size idx grid
    );
    (ball_radius, Kh.grid_size grid, fill_time)
  in
  let fp = P.create_ps_file ("test/" ^ filename) in
(*   let ball_radii = [3.0; 4.0; 4.5; 5.0; 5.5; 6.0; 6.5; 6.75; 7.0; 7.25; 7.5] in *)
let ball_radii = [3.0; 4.0; 4.5; 5.0; 5.5; 6.0; 6.5; 6.75; 7.0; 7.25; 7.5; 7.75; 8.0; 8.25; 8.5; 8.75] in
  Printf.printf "kd\n";
  let kd_pts = tail_mapi (fun i r -> Printf.printf "%f " r; flush stdout; build_kd_pts i r) ball_radii in
(*
  Printf.printf "naive\n";
  let naive_pts = tail_mapi (fun i r -> Printf.printf "%f\n" r; flush stdout; build_naive_pts i r) ball_radii in
*)
  let graph_settings = { P.default_graph with
(*
    P.xmax = max_in_list_int (List.map second_triple naive_pts);
    P.ymax = max_in_list_float (List.map third_triple naive_pts);
*)
    P.xmax = float_of_int (max_in_list_int (List.map second_triple kd_pts));
    P.ymax = max_in_list_float (List.map third_triple kd_pts);
    P.ylabeloffsets = 22;
    P.write_thresholds = true;
  } in
  let print_info kd_pts naive_pts =
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
(*
  print_info kd_pts naive_pts;
  P.draw_graph fp graph_settings [(P.green, kd_pts); (P.red, naive_pts)];
*)
  P.draw_graph fp graph_settings [(P.green, kd_pts)] "NO. OF POINTS" "TIME (s)";
  P.close_ps_file fp

let run_para_test filename print_output =
  let settings = { P.default with
    P.scale = 10.0;
    P.xorigin = 20;
    P.yorigin = 20;
  } in
  let plot_one_grid newfilename threshold to_list grid_size idx grid =
    let thresh_str = Str.global_replace (Str.regexp_string ".") "-" (sprintf "%g" threshold) in
    let fp = P.create_ps_file (sprintf "out/%s--%s" newfilename thresh_str) in
    P.plot_grid fp settings (to_list grid);
    output_string fp (sprintf "30 750 moveto (THRESHOLD: %f) show\n" threshold);
    output_string fp (sprintf "30 735 moveto (NO. POINTS: %d) show\n" (grid_size grid));
    P.close_ps_file fp
  in
  let build_naive_pts idx threshold =
    let start_time = Sys.time () in
    let (grid, _) = N.fill_space threshold (26.5, 24.5) in
    let fill_time = Sys.time () -. start_time in
    if print_output then plot_one_grid (filename ^ "naive") threshold (fun g -> List.map (fun p -> T.to_screen p threshold) (N.to_list g)) N.grid_size idx grid;
    (threshold, N.grid_size grid, fill_time)
  in
  let build_kd_pts idx threshold =
    let start_time = Sys.time () in
    let (grid, _) = K.fill_space threshold (26.5, 24.5) in
    let fill_time = Sys.time () -. start_time in
    if print_output then plot_one_grid (filename ^ "kd") threshold (fun g -> List.map (fun p -> T.to_screen p threshold) (K.to_list g)) K.grid_size idx grid;
    (threshold, K.grid_size grid, fill_time)
  in
  let max_in_list_float = List.fold_left max min_float in
  let max_in_list_int = List.fold_left max min_int in
  let thresholds = [0.27; 0.28; 0.29; 0.30; 0.31; 0.315; 0.32; 0.325; 0.33; 0.34; 0.36; 0.38; 0.40; 0.42; 0.44; 0.46; 0.48; 0.5; 0.52; 0.54; 0.55; 0.57; 0.6; 0.7; 0.8; 1.; 2.; 3.; 4.; 5.] in
(* let thresholds = [0.5; 0.515; 0.52; 0.53; 0.54; 0.55; 0.6; 0.62; 0.65; 0.7; 0.75; 0.8; 0.9; 1.; 1.1; 1.2; 1.3; 1.4; 1.5; 2.; 3.; 4.; 5.] in *)
  (* let thresholds = [0.49; 0.5; 0.505; 0.51; 0.515; 0.52; 0.53; 0.54; 0.55; 0.6; 0.62; 0.65; 0.7; 0.75; 0.8; 0.9; 1.; 1.1; 1.2; 1.3; 1.4; 1.5; 2.; 3.; 4.; 5.] in *)
  let fp = P.create_ps_file ("test/" ^ filename) in
  Printf.printf "kd\n";
  let kd_pts = tail_mapi (fun i t -> Printf.printf "%f\n" t; flush stdout; build_kd_pts i t) thresholds in
(*
  Printf.printf "naive\n";
  let naive_pts = tail_mapi (fun i t -> Printf.printf "%f\n" t; flush stdout; build_naive_pts i t) thresholds in
*)
  let graph_settings = { P.default_graph with
(*
    P.xmax = max_in_list_int (List.map second_triple naive_pts);
    P.ymax = max_in_list_float (List.map third_triple naive_pts);
*)
    P.xmax = float_of_int (max_in_list_int (List.map second_triple kd_pts));
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
  P.draw_graph fp graph_settings [(P.green, kd_pts)] "NO. OF POINTS" "TIME (s)";
  P.close_ps_file fp

