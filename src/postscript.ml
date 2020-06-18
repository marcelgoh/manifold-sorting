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

let file = "/out/plot"
let preamble ="
%!PS
/dot { 1.5 0 360 arc closepath fill } def
/circle { 0 360 arc closepath stroke } def
/Courier findfont 9 scalefont setfont
/ytick { newpath 0 exch moveto -5 0 rmoveto 10 0 rlineto stroke} def
/xtick { newpath 0 moveto 0 -5 rmoveto 0 10 rlineto stroke} def
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

let set_colour fp (r, g, b) = output_string fp (sprintf "%f %f %f setrgbcolor\n" r g b)

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
    | x :: y :: p' ->
        let t = (float_of_int idx) /. num_pts in
        let r = r1 +. (r2 -. r1) *. t in
        let g = g1 +. (g2 -. g1) *. t in
        let b = b1 +. (b2 -. b1) *. t in
        if idx == 0 then set_colour fp stgs.startcolour else if idx == ((List.length grid_list) - 1) then set_colour fp stgs.endcolour else set_colour fp (r, g, b);
        output_string fp (sprintf "%f %f dot\n" (x *. stgs.scale) (y *. stgs.scale));
        if stgs.draw_circle <> 0.0 then (
          let f a = a *. stgs.scale in
          output_string fp (sprintf "%f %f %f circle\n" (f x) (f y) (f stgs.draw_circle))
        )
    | _ -> raise Postscript_error
  in
  List.iteri print_point grid_list;  (* print every point in the grid *)
  set_colour fp black
