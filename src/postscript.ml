(* PostScript-generating capabilities *)

open Printf

module D = DynArray
module N = Naive_euclidean

let red = (1.0, 0.0, 0.0)
let green = (0.101, 0.501, 0.0)
let blue = (0.0, 0.0, 1.0)
let black = (0.0, 0.0, 0.0)
let purple = (0.5, 0.0, 1.0)

let file = "/out/plot"
let preamble = "%!PS\n/dot { 1.5 0 360 arc closepath fill } def\n"

(* create_ps_file filename creates (overwrites) <filename>.ps *)
let create_ps_file filename = open_out (filename ^ ".ps")

let plot_grid (grid : N.grid) xscale yscale colour1 colour2 filename =
  let (r1, g1, b1) = colour1 in
  let (r2, g2, b2) = colour2 in
  let fp = create_ps_file filename in
  let num_pts = float_of_int (N.grid_size grid) in
  output_string fp preamble;
  output_string fp "306 396 translate\n0.5 setlinewidth\n";  (* put origin at centre *)
  output_string fp "newpath 0 -396 moveto 0 396 lineto stroke\n"; (* x-axis *)
  output_string fp "newpath -306 0 moveto 306 0 lineto stroke\n"; (* y-axis *)
  let set_colour (r, g, b) = output_string fp (sprintf "%f %f %f setrgbcolor\n" r g b) in
  let print_point idx (x, y) =
    let t = (float_of_int idx) /. num_pts in
    let r = r1 +. (r2 -. r1) *. t in
    let g = g1 +. (g2 -. g1) *. t in
    let b = b1 +. (b2 -. b1) *. t in
    set_colour (r, g, b);
    output_string fp (sprintf "%f %f dot\n" (x *. xscale) (y *. yscale))
  in
  D.iteri print_point grid;  (* print every point in the grid *)
  output_string fp "showpage\n";
  close_out fp;
