(* PostScript-generating capabilities *)

open Printf

module D = DynArray
module N = Naive_euclidean

let file = "/out/plot"
let preamble = "%!PS\n/dot { 1.5 0 360 arc closepath fill } def\n"

(* create_ps_file filename creates (overwrites) <filename>.ps *)
let create_ps_file filename = open_out_gen [Open_trunc] 511 ("filename" ^ ".ps")

let plot_grid (grid : N.grid) filename =
  let fp = create_ps_file filename in
  output_string fp preamble;
  let print_point (x, y) = output_string fp (sprintf "%f %f dot\n" x y) in
  D.iter print_point grid;  (* print every point in the grid *)
  output_string fp "showpage\n";
  close_out fp;
