(* PostScript-generating capabilities *)

open Printf

module D = DynArray
module K = Kd_euclidean

exception Postscript_error

let red = (1.0, 0.0, 0.0)
let orange = (1.0, 0.3, 0.0)
let green = (0.101, 0.501, 0.0)
let blue = (0.0, 0.0, 1.0)
let black = (0.0, 0.0, 0.0)
let purple = (0.5, 0.0, 1.0)

type colour = float * float * float

type settings = {
  scale : float;
  colour1 : colour;
  colour2 : colour;
  startcolour : colour;
  endcolour : colour;
  xorigin : int;
  yorigin : int;
  draw_circle : float;  (* radius of circle to draw *)
}

type graph_settings = {
  xmax : int;
  ymax : float;
  xnumticks : int;
  ynumticks : int;
  xmargin : int;  (* (xmargin, ymargin) is the origin; an equal amount of space is ... *)
  ymargin : int;  (* left at the top and right side of the graph as well *)
}

let default = {
  scale = 5.0;
  colour1 = red;
  colour2 = blue;
  startcolour = green;
  endcolour = red;
  xorigin = 306;
  yorigin = 396;
  draw_circle = 0.0;
}

let default_graph = {
  xmax = 5000;
  ymax = 50.0;
  xnumticks = 5;
  ynumticks = 5;
  xmargin = 30;
  ymargin = 30;
}

let file = "/out/plot"
let preamble ="
%!PS
/dot { 1.5 0 360 arc closepath fill } def
/circle { 0 360 arc closepath stroke } def
/Courier findfont 9 scalefont setfont
/ytick { newpath 0 exch moveto -5 0 rmoveto 10 0 rlineto stroke} def
/xtick { newpath 0 moveto 0 -5 rmoveto 0 10 rlineto stroke} def
0.5 setlinewidth
"

let xtick fp stgs (tick : int) =
  let x = float_of_int tick *. stgs.scale in
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

let show_float fp flt = output_string fp (sprintf "(%g ) show\n" flt)
let show_int fp n = output_string fp (sprintf "(%d ) show\n" n)
let show_str fp s = output_string fp (sprintf "(%s ) show\n" s)
let moveto fp x y = output_string fp (sprintf "%d %d moveto " x y)
let dot fp x_f y_f = output_string fp (sprintf "newpath %f %f dot\n" x_f y_f)

let set_colour fp (r, g, b) = output_string fp (sprintf "%f %f %f setrgbcolor\n" r g b)

let draw_axes fp gstgs =
  output_string fp (sprintf "newpath %d %d moveto 0 %d rlineto stroke\n"
                      gstgs.xmargin gstgs.ymargin (792 - (2 * gstgs.ymargin)));
  output_string fp (sprintf "newpath %d %d moveto %d 0 rlineto stroke\n"
                      gstgs.xmargin gstgs.ymargin (612 - (2 * gstgs.xmargin)))

let xlabel fp gstgs offset str =
  moveto fp gstgs.xmargin (gstgs.ymargin - offset);
  show_str fp str

let ylabel fp gstgs offset str =
  output_string fp (sprintf "gsave %d %d translate 90 rotate\n" (gstgs.xmargin - offset) gstgs.ymargin);
  moveto fp 0 0;
  show_str fp str;
  output_string fp "grestore\n"

let draw_graph fp gstgs point_list_list =
  let xmar = float_of_int gstgs.xmargin in
  let ymar = float_of_int gstgs.ymargin in
  draw_axes fp gstgs;
  xlabel fp gstgs 20 "     NO. OF POINTS";
  ylabel fp gstgs 15 "     TIME (s)";
  (* scale factors *)
  let xf = (612.0 -. (2.0 *. xmar)) /. (float_of_int gstgs.xmax) in
  let yf = (792.0 -. (2.0 *. ymar)) /. gstgs.ymax in
  let draw (_, x',y) =
    let x = float_of_int x' in
    dot fp (x *. xf +. xmar) (y *. yf +. ymar)
  in
  let draw_one_list (col, l) =
    set_colour fp col;
    List.iter draw l
  in
  List.iter draw_one_list point_list_list;
  set_colour fp black

let plot_grid fp stgs grid_list =
  let (r1, g1, b1) = stgs.colour1 in
  let (r2, g2, b2) = stgs.colour2 in
  let (xo, yo) = (stgs.xorigin, stgs.yorigin) in
  let num_pts = float_of_int (List.length grid_list) in
  output_string fp (sprintf "%d %d translate\n0.5 setlinewidth\n" xo yo);
  output_string fp (sprintf "newpath 0 -%d moveto 0 %d lineto stroke\n" yo (yo + 792));
  output_string fp (sprintf "newpath -%d 0 moveto %d 0 lineto stroke\n" xo (xo + 612));
  let print_point idx p =
    match p with
    | x :: y :: _ ->
        let t = (float_of_int idx) /. num_pts in
        let r = r1 +. (r2 -. r1) *. t in
        let g = g1 +. (g2 -. g1) *. t in
        let b = b1 +. (b2 -. b1) *. t in
        if idx == 0 then
          set_colour fp stgs.startcolour
        else if idx == ((List.length grid_list) - 1) then
          set_colour fp stgs.endcolour
        else
          set_colour fp (r, g, b);
        output_string fp (sprintf "%f %f dot\n" (x *. stgs.scale) (y *. stgs.scale));
        if stgs.draw_circle <> 0.0 then (
          let f a = a *. stgs.scale in
          output_string fp (sprintf "%f %f %f circle\n" (f x) (f y) (f stgs.draw_circle))
        )
    | _ -> raise Postscript_error
  in
  List.iteri print_point grid_list;  (* print every point in the grid *)
  set_colour fp black
