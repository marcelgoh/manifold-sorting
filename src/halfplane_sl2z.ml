module Halfplane_sl2z : Space.Space with type point = float * float = struct
  exception Sl2z_error

  type point = float * float
  type quad = float * float * float * float   (* matrix entries a b c d *)
  type gamma = float * float * float * float  (* group element gamma in SL_2(ZZ) *)

  (* because quad positive-definite symmetric, this should return two real values *)
  let eig ((a,b,c,d) : quad) =
    let (bb, cc) = ((-. (a +. d)), a *. d -. b *. c) in
    let discriminant = sqrt (bb ** 2.0 -. 4.0 *. cc) in
    (((-. bb) +. discriminant) /. 2.0, ((-. bb) -. discriminant) /. 2.0)

  (* determinant *)
  let det ((a,b,c,d) : gamma) = a *. d -. b *. c
  (* inverse -- of quad or gamma *)
  let inv (a,b,c,d) =
    let dd = det (a,b,c,d) in
    if dd = 0.0 then
      raise Sl2z_error
    else
      let f a = a /. dd in
      (f d, f (-. b), f (-. c), f a)
  let multiply (a, b, c, d) (e, f, g, h) =
    (a *. e +. b *. g, a *. f +. b *. h, c *. e +. d *. g, c *. f +. d *. h)

  (* map a point x + iy to the quadratic form (1/y)X^2 - 2x/y XY + (x^2/y + y) Y^2 *)
  let quad_of_point (x, y) =
    (1.0 /. y, ((-.x) /. y), ((-.x) /. y), x ** 2.0 /. y +. y)
  (* inverse of the above *)
  let point_of_quad (a,b,c,d) =
    let y = 1.0 /. a in
    (sqrt ((d -. y) *. y), y)

  (* finds unique matrix that takes basis b1, b2 to basis v1, v2 *)
  let find_matrix (b1x, b1y) (b2x, b2y) (v1x, v1y) (v2x, v2y) =
    let det = (b1x *. b2y) -. (b2x *. b1y) in
    ((b2y*.v1x)-.(b2x*.v1y), (b1x*.v1y)-.(b1y*.v1x), (b2y*.v2x)-.(b2x*.v2y), (b1x*.v2y)-.(b1y*.v2x))

  let dist (x1, y1) (x2, y2) =
    let q1 = quad_of_point (x1, y1) in
    let q2 = quad_of_point (x2, y2) in
(*
    let (a1, b1, c1, d1) = q1 in
    let (a2, b2, c2, d2) = q2 in
    Printf.printf "q1, q2 = (%f %f %f %f), (%f %f %f %f)\n" a1 b1 c1 d1 a2 b2 c2 d2;
*)
    let bigA = multiply (inv q1) q2 in
    let (l1, l2) = eig bigA in
    let l1' = Float.abs (log l1) in
    let l2' = Float.abs (log l2) in
    let q_dist = if l1' > l2' then l1' else l2' in
    q_dist  (* currently not the correct output *)

  let simpl p = p

  let offset_list =
    [
      (0., 0.4);
      (0., 2.5);
      (0.35, 0.55);
      (-0.35, 0.55);
      (0.8, 0.8);
      (-0.8, 0.8);
      (0.8, 1.5);
      (-0.8, 1.5);
    ]

  let get_local_cover r (x, y) =
    if r = 0.5 then
      List.map (fun (x', y') -> (x +. (x' *. y), y *. y')) offset_list
    else raise Sl2z_error

  let to_screen (x, y) r = (x, y *. cosh r), (y *. sinh r), (x, y)
end
