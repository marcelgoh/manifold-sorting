(* PostScript-generating capabilities *)

open Printf

module D = DynArray

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
}

type graph_settings = {
  xmax : float;
  ymax : float;
  xnumticks : int;
  ynumticks : int;
  xmargin : int;  (* (xmargin, ymargin) is the origin; an equal amount of space is ... *)
  ymargin : int;  (*   left at the top and right side of the graph as well *)
  xaxislabeloffset : int;
  yaxislabeloffset : int;
  xlabeloffsets : int;
  ylabeloffsets : int;
  write_thresholds : bool;
}

let default = {
  scale = 5.0;
  colour1 = red;
  colour2 = blue;
  startcolour = green;
  endcolour = red;
  xorigin = 306;
  yorigin = 396;
}

let default_graph = {
  xmax = 5000.0;
  ymax = 50.0;
  xnumticks = 5;
  ynumticks = 5;
  xmargin = 30;
  ymargin = 30;
  xaxislabeloffset = 20;  (* text that labels the axis *)
  yaxislabeloffset = 15;
  xlabeloffsets = 8;  (* text beside a tickmark *)
  ylabeloffsets = 18;
  write_thresholds = false;
}

let file = "/out/plot"
let preamble ="
%!PS
/dot { 1.5 0 360 arc closepath fill } def
/circle { 0 360 arc closepath stroke } def
/Courier findfont 9 scalefont setfont
/ytick { newpath moveto -5 0 rmoveto 10 0 rlineto stroke} def
/xtick { newpath moveto 0 -5 rmoveto 0 10 rlineto stroke} def
0.5 setlinewidth
"

let set_fontsize fp size = output_string fp (sprintf "/Courier findfont %d scalefont setfont\n" size)

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
(* draws a line segment *)
let draw_line fp x1 y1 x2 y2 =
  output_string fp (sprintf "newpath %f %f moveto " x1 y1);
  output_string fp (sprintf "%f %f lineto stroke\n" x2 y2)
let dot fp x_f y_f = output_string fp (sprintf "newpath %f %f dot\n" x_f y_f)

let set_colour fp (r, g, b) = output_string fp (sprintf "%f %f %f setrgbcolor\n" r g b)

let draw_axes fp gstgs =
  output_string fp (sprintf "newpath %d %d moveto 0 %d rlineto stroke\n"
                      gstgs.xmargin gstgs.ymargin (792 - gstgs.ymargin));
  output_string fp (sprintf "newpath %d %d moveto %d 0 rlineto stroke\n"
                      gstgs.xmargin gstgs.ymargin (612 - gstgs.xmargin))

let xlabel fp gstgs str =
  moveto fp gstgs.xmargin (gstgs.ymargin - gstgs.xaxislabeloffset);
  show_str fp str

let ylabel fp gstgs str =
  output_string fp (sprintf "gsave %d %d translate 90 rotate\n"
                            (gstgs.xmargin - gstgs.yaxislabeloffset) gstgs.ymargin);
  moveto fp 0 0;
  show_str fp str;
  output_string fp "grestore\n"

(* print an int value on the x-axis *)
let xtick fp xf gstgs (n : int) =
  let y' = gstgs.ymargin - gstgs.xlabeloffsets in  (* offset the label *)
  let x = float_of_int n *. gstgs.xmax /. float_of_int gstgs.xnumticks in
  let coord_x = x *. xf +. float_of_int gstgs.xmargin in
  output_string fp (sprintf "%f %d xtick\n" coord_x gstgs.ymargin);
  output_string fp (sprintf "%f %d moveto (%d) show\n" (coord_x +. 2.0) y' (int_of_float x))

(* print an int value on the x-axis *)
let ytick fp yf gstgs (n : int) =
  let x' = gstgs.xmargin - gstgs.ylabeloffsets in  (* offset the label *)
  let y = float_of_int n *. gstgs.ymax /. float_of_int gstgs.ynumticks in
  let coord_y = y *. yf +. float_of_int gstgs.ymargin in
  output_string fp (sprintf "%d %f ytick\n" gstgs.xmargin coord_y);
  output_string fp (sprintf "%d %f moveto (%d) show\n" x' (coord_y +. 3.0) (int_of_float y))

let draw_graph fp gstgs point_list_list xstr ystr =
  let xmar = float_of_int gstgs.xmargin in
  let ymar = float_of_int gstgs.ymargin in
  let rec range m n = if m = n then [m] else m :: range (m+1) n in
  let xticks = range 0 gstgs.xnumticks in
  let yticks = range 0 gstgs.ynumticks in
  draw_axes fp gstgs;
  xlabel fp gstgs ("     " ^ xstr);
  ylabel fp gstgs ("     " ^ ystr);
  (* scale factors *)
  let xf = (612.0 -. (2.0 *. xmar)) /. gstgs.xmax in
  let yf = (792.0 -. (2.0 *. ymar)) /. gstgs.ymax in
  let draw col (threshold, x',y') =
    let x = float_of_int x' *. xf +. xmar in
    let y = y' *. yf +. ymar in
    set_colour fp col;
    dot fp x y;
    if gstgs.write_thresholds then (
      moveto fp (int_of_float x + 2) (int_of_float y + 2);
      set_fontsize fp 6;
      set_colour fp black;
      show_float fp threshold;
    )
  in
  let draw_one_list (col, l) = List.iter (draw col) l in
  let draw_one_xtick = xtick fp xf gstgs in
  let draw_one_ytick = ytick fp yf gstgs in
  List.iter draw_one_xtick xticks;
  List.iter draw_one_ytick yticks;
  List.iter draw_one_list point_list_list;
  set_colour fp black

let scatterplot fp gstgs points =
  let xmar = float_of_int gstgs.xmargin in
  let ymar = float_of_int gstgs.ymargin in
  let rec range m n = if m = n then [m] else m :: range (m+1) n in
  let xticks = range 0 gstgs.xnumticks in
  let yticks = range 0 gstgs.ynumticks in
  draw_axes fp gstgs;
  (* scale factors *)
  let xf = (612.0 -. (2.0 *. xmar)) /. gstgs.xmax in
  let yf = (792.0 -. (2.0 *. ymar)) /. gstgs.ymax in
  let draw col (x',y') =
    let x = x' *. xf +. xmar in
    let y = y' *. yf +. ymar in
    set_colour fp col;
    dot fp x y;
  in
  let draw_one_list (col, l) = List.iter (draw col) l in
  let draw_one_xtick = xtick fp xf gstgs in
  let draw_one_ytick = ytick fp yf gstgs in
  List.iter draw_one_xtick xticks;
  List.iter draw_one_ytick yticks;
  List.iter draw_one_list points;
  set_colour fp black

let draw_point fp stgs ((x, y), r, (x', y')) =
  output_string fp (sprintf "%f %f dot\n" (x' *. stgs.scale) (y' *. stgs.scale));
  if r <> 0.0 then (
    let f a = a *. stgs.scale in
    output_string fp (sprintf "%f %f %f circle\n" (f x) (f y) (f r))
  )

let draw_line_segment fp stgs (x1, y1) (x2, y2) =
  if x1 > 1000. || y1 > 1000. || x2 > 1000. || y2 > 1000.
  || x1 < (-1000.) || y1 < (-1000.) || x2 < (-1000.) || y2 < (-1000.)
  then
    ()
  else (
    let fx x = (x *. stgs.scale) +. (float_of_int stgs.xorigin) in
    let fy y = (y *. stgs.scale) +. (float_of_int stgs.yorigin) in
    draw_line fp (fx x1) (fy y1) (fx x2) (fy y2)
  )

let plot_edges fp stgs (vertices, edges) =
  let point_arr = Array.make (List.length vertices) (0.0, 0.0) in
  List.iter (fun (i, (x,y)) ->
    Array.set point_arr i (x,y)) vertices;
  let draw_edge (i, j) = draw_line_segment fp stgs point_arr.(i) point_arr.(j) in
  List.iter draw_edge edges

let plot_grid fp stgs grid_list =
  let (r1, g1, b1) = stgs.colour1 in
  let (r2, g2, b2) = stgs.colour2 in
  let (xo, yo) = (stgs.xorigin, stgs.yorigin) in
  let num_pts = float_of_int (List.length grid_list) in
  output_string fp (sprintf "%d %d translate\n0.5 setlinewidth\n" xo yo);
  output_string fp (sprintf "newpath 0 -%d moveto 0 %d lineto stroke\n" yo (yo + 792));
  output_string fp (sprintf "newpath -%d 0 moveto %d 0 lineto stroke\n" xo (xo + 612));
  let print_point idx p =
    (* this takes a triple from to_screen *)
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
    draw_point fp stgs p
  in
  List.iteri print_point grid_list;  (* print every point in the grid *)
  set_colour fp black
