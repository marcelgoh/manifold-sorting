(* PostScript-generating capabilities *)

open Printf

module D = DynArray
module N = Naive_euclidean

let file = "/out/plot"
let preamble = "%!PS\n/dot { 1.5 0 360 arc closepath fill } def\n"

(* create_ps_file filename creates (overwrites) <filename>.ps *)
let create_ps_file filename = open_out (filename ^ ".ps")

let plot_grid (grid : N.grid) xscale yscale filename =
  let fp = create_ps_file filename in
  output_string fp preamble;
  output_string fp "306 396 translate\n0.5 setlinewidth\n";  (* put origin at centre *)
  output_string fp "newpath 0 -396 moveto 0 396 lineto stroke\n"; (* x-axis *)
  output_string fp "newpath -306 0 moveto 306 0 lineto stroke\n"; (* y-axis *)
  let print_point (x, y) =
    output_string fp (sprintf "%f %f dot\n" (x *. xscale) (y *. yscale)) in
  D.iter print_point grid;  (* print every point in the grid *)
  output_string fp "showpage\n";
  close_out fp;
