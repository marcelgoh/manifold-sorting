(* Main entry point of the program *)

open Printf

module M =  Metric
module P = Postscript
module T = Test
module H = Halfplane.Halfplane
module K = Kd.Kd(H)(struct let to_e p = [H.dist p (1., 0.5); H.dist p (-1., 0.5)] end)
module N = Naive.Naive(H)

let _ =
  (* T.run_para_test "kdonly1" true; *)
  let fp = P.create_ps_file "halfplane" in
  let grid = K.fill_ball (0., 1.) 2.0 0.5 (0., 1.) in
  P.plot_grid fp { P.default with scale=30. } (List.map (fun p -> H.to_screen p 0.5) (K.to_list grid));
  P.close_ps_file fp
