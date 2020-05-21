(* Main entry point of the program *)

open Printf

module M =  Metric
module N = Naive_euclidean
module P = Postscript

let main () =
  let grid = N.create_grid () in
  N.add_to_grid grid (1.2, 2.4);
  N.add_to_grid grid (3.1, 5.3);
  N.add_to_grid grid (1.0, 1.0);
  N.add_to_grid grid (0.0, 0.0);
  P.plot_grid grid 50.0 50.0 "out/grid"

let _ = main ()
