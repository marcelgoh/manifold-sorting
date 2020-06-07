(* PostScript-generating capabilities *)

open Printf

module D = DynArray
module N = Naive_euclidean

let red = (1.0, 0.0, 0.0)
let green = (0.101, 0.501, 0.0)
let blue = (0.0, 0.0, 1.0)
let black = (0.0, 0.0, 0.0)
let purple = (0.5, 0.0, 1.0)

type settings = {
  xscale : float;
  yscale : float;
  colour1 : float * float * float;
  colour2 : float * float * float;
  xorigin : int;
  yorigin : int;
}

let default = {
  xscale = 5.0;
  yscale = 5.0;
  colour1 = red;
  colour2 = blue;
  xorigin = 306;
  yorigin = 396;
}

let file = "/out/plot"
let preamble ="
%!PS
/dot { 1.5 0 360 arc closepath fill } def
/Courier findfont 9 scalefont setfont
/ytick { newpath 0 exch moveto -5 0 rmoveto 10 0 rlineto stroke} def
/xtick { newpath 0 moveto 0 -5 rmoveto 0 10 rlineto stroke} def
"

let xtick fp stgs (tick : int) =
  let x = float_of_int tick *. stgs.xscale in
  output_string fp (sprintf "%f xtick\n" x);
  output_string fp (sprintf "%f -7 moveto (%d) show\n" x tick)

(* create_ps_file filename creates (overwrites) <filename>.ps *)
let create_ps_file filename =
  let fp = open_out (filename ^ ".ps") in
  output_string fp preamble;
  fp

let close_ps_file fp =
  output_string fp "showpage\n";
  close_out fp

let plot_grid fp stgs grid =
  let (r1, g1, b1) = stgs.colour1 in
  let (r2, g2, b2) = stgs.colour2 in
  let (xo, yo) = (stgs.xorigin, stgs.yorigin) in
  let num_pts = float_of_int (N.grid_size grid) in
  output_string fp (sprintf "%d %d translate\n0.5 setlinewidth\n" xo yo);
  output_string fp (sprintf "newpath 0 -%d moveto 0 %d lineto stroke\n" yo (yo + 792));
  output_string fp (sprintf "newpath -%d 0 moveto %d 0 lineto stroke\n" xo (xo + 612));
  let set_colour (r, g, b) = output_string fp (sprintf "%f %f %f setrgbcolor\n" r g b) in
  let print_point idx (x, y) =
    let t = (float_of_int idx) /. num_pts in
    let r = r1 +. (r2 -. r1) *. t in
    let g = g1 +. (g2 -. g1) *. t in
    let b = b1 +. (b2 -. b1) *. t in
    set_colour (r, g, b);
    output_string fp (sprintf "%f %f dot\n" (x *. stgs.xscale) (y *. stgs.yscale))
  in
  D.iteri print_point grid;  (* print every point in the grid *)
  set_colour black
