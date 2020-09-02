(* Main entry point of the program *)

open Printf

module M = Metric
module P = Postscript
module T = Test
module H = Halfplane.Halfplane
module N = Naive.Naive(H)
module Hs = Halfplane_sl2z.Halfplane_sl2z
module Hl = Halfplane_soql.Halfplane_soql

let _ =
(*   T.run_para_test "kdandnaive" true *)

  (* let fp = P.create_ps_file "halfplane" in
   * let rec range n = if n < 1. then [] else range (n -. 1.) @ [n -. 1.] in
   * let e = exp 2. in
   * let e2 = e ** 2. in
   * let n = 1000. in
   * let pts = List.map (fun i ->
   *               let t = i *. (acos (-.1.)) /. n in
   *               let s = sin t in
   *               let c = cos t in
   *               let d = c ** 2. +. e2 *. (s ** 2.) in
   *               (e2 *. s *. c /. d, e /. d)) (range n) in
   * (\* let dists = List.map (fun p -> (H.dist p (0., exp (-2.)), H.dist p (0., exp 2.))) pts in *\)
   * (\* let dists = List.map (fun p -> (H.dist p (1., 0.5), H.dist p (-1., 0.5))) pts in *\)
   * let dists = List.map (fun (x, y) ->
   *                 let r = H.dist (x, y) (0., 1.) in
   *                 let theta = atan2 (y -. 1.) (x *. y) in
   *                 (r *. cos theta, r *. sin theta)) pts in
   * P.scatterplot fp {P.default_graph with xmax = 10.; ymax = 10.; xmargin = 100; ymargin = 100} (List.map (fun p -> ((0., 0., 1.), [p])) dists);
   * (\* let grid = K.fill_ball (0., 1.) 1.0 0.5 (0., 1.) in
   *  * P.plot_grid fp { P.default with scale=15. } (List.map (fun p -> H.to_screen p 0.5) (K.to_list grid));
   *  * P.plot_grid fp { P.default with scale=15. } [H.to_screen (0., 1.) 0.5]; *\)
   * P.close_ps_file fp; *)
(*   Test.run_halfplane_gamma_test "halfplanegamma" true *)

(*
let (h_dist, _) = Hs.dist (0., 0.48) (0., 3.) in
Printf.printf "halfplane distance: %f\n" h_dist;
*)
  Utils.small_vector_denom := 100.;
  Utils.halfplane_sl2z_bigN := 4;
(*   Hs.dist (0., 0.48) (0., 3.) *)
  Test.run_halfplane_gamma_test "halfplanegammamod4" true

(*   Test.run_halfplane_test "voronoi" true *)
(*   Test.fill_euclidean_ball "euclideanball" 0.5 true; *)
(*
  let x1, y1 = List.nth (Hs.get_local_cover 0.5 (List.nth (Hs.get_local_cover 0.5 (0.0, 3.0)) 0)) 1 in
  Hs.dist (x1, y1) (0.0, 3.0);
*)
(*
let (dist, glc) = Hs.dist (203.293250, 1342.703510) (386.257174, 1342.703510) in
Printf.printf "distance = %f\t glc = %s\n" dist (if glc then "true" else "false")
*)
(*   Test.halfplane_comp_test "hi" 9.0 *)
