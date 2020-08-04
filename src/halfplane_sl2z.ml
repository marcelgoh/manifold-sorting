module Halfplane_sl2z : Space.Space with type point = float * float = struct
  exception Sl2z_error of string

  type point = float * float
  type quad = float * float * float * float   (* matrix entries a b c d *)
  type gamma = float * float * float * float  (* group element gamma in SL_2(ZZ) *)

  (* matrix acts on point by mobius transformation *)
  let mobius (a, b, c, d) (x,y) =
    let module C = Complex in
    let f rr : C.t = { re=rr; im=0.0 } in  (* complex_of_real *)
    let z : C.t = { re=x; im=y } in
    let (a', b', c', d') = (f a, f b, f c, f d) in
    let z' = C.div (C.add (C.mul a' z) b') (C.add (C.mul c' z) d') in
    (z'.re, z'.im)

  (* returns list of integers from a to b, inclusive *)
  let range a b =
    let rec range' a b acc =
      if a = b then b :: acc else range' a (b-1) (b::acc)
    in
    range' a b []

  let transpose (a, b, c, d) = (a, c, b, d)

  (* cartesian product of two lists *)
  let product l1 l2 =
    List.rev (List.fold_left (fun x a -> List.fold_left (fun y b -> (a,b)::y) x l2) [] l1)

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
      raise (Sl2z_error "Zero determinant.")
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

  let apply_quad (a, b, _, c) (x, y) = a*.x*.x +. 2.0*.b*.x*.y +. c*.y*.y
  let mat_vec_mul (a, b, c, d) (x, y) = (a*.x +. b*.y, c*.x +. d*.y)

  (* finds unique matrix that takes basis b1, b2 to basis v1, v2 *)
  let find_matrix (b1x, b1y) (b2x, b2y) (v1x, v1y) (v2x, v2y) =
    let det = (b1x *. b2y) -. (b2x *. b1y) in
    let f x = x /. det in
    let a = f ((b2y*.v1x)-.(b1y*.v2x)) in
    let b = f ((b1x*.v2x)-.(b2x*.v1x)) in
    let c = f ((b2y*.v1y)-.(b1y*.v2y)) in
    let d = f ((b1x*.v2y)-.(b2x*.v1y)) in
(*
    Printf.printf "(%.1f %.1f %.1f %.1f) takes (%.1f %.1f) to (%.1f %.1f) and (%.1f %.1f) to (%.1f %.1f).\n"
    a b c d b1x b1y v1x v1y b2x b2y v2x v2y;
*)
    (a, b, c, d)

  (* returns distance if it is <= 0.5, otherwise returns max_float *)
  let dist (x1, y1) (x2, y2) : float * bool =
    let q1 = quad_of_point (x1, y1) in
    let q2 = quad_of_point (x2, y2) in
    let (q1a, q1b, q1c, q1d) = q1 in let (q2a, q2b, q2c, q2d) = q2 in
    Printf.printf "Quadratic form for point (%.1f, %.1f) is %fx^2 + %fxy + %fy^2.\n" x1 y1 q1a (q1b +. q1c) q1d;
    Printf.printf "Quadratic form for point (%.1f, %.1f) is %fx^2 + %fxy + %fy^2.\n" x2 y2 q2a (q2b +. q2c) q2d;
    let start_b1 = (1.0, 0.0) in
    let start_b2 = (0.0, 1.0) in
    (* === here we would reduce b1, b2 with respect to q2 via LLL === *)
    let b1 = start_b1 in
    let b2 = start_b2 in
    (* === end temporary code === *)
    let ((b11, b12), (b21, b22)) = (b1, b2) in
    (* compute bounds for v_i: e^(-1)q1(b1) <= q2(v1) <= e*q1(b1) and e^(-1)q1(b2) <= q2(v2) <= e*q1(b2) *)
    let (rad1, rad2, small_rad1, small_rad2) =
      let q1b1 = apply_quad q1 b1 in
      let q1b2 = apply_quad q1 b2 in
      let ee = exp 1.0 in
      let small_ee = exp (-.1.0) in
      (ee*.q1b1, ee*.q1b2, small_ee*.q1b1, small_ee*.q1b2)
    in
    Printf.printf "rad1: %f, rad2: %f, small_rad1: %f, small_rad2: %f\n" rad1 rad2 small_rad1 small_rad2;
    let (qa, qb, qb', qc) = q2 in
    if qb <> qb' then raise (Sl2z_error "Quadratic form is not symmetric.");
    (* coefficients of quadratic form *)
    let qa' = qa *.b11*.b11 +. 2.0*.qb*.b11*.b12 +. qc*.b12*.b12 in
    let qb' = 2.0*.(qa*.b11*.b21 +. qb*.(b21*.b12 +. b11*.b22) +. qc*.b12*.b22) in
    let qc' = qa *.b21*.b21 +. 2.0*.qb*.b21*.b22 +. qc*.b22*.b22 in
(*     Printf.printf "New quadratic form is %fx^2 + %fxy + %fy^2.\n" qa' qb' qc'; *)
    let max_rad = if rad1 > rad2 then rad1 else rad2 in
    (* solve Lagrange multiplier problem *)
    let qb'2 = qb'*.qb' in
    let max_m = truncate (sqrt (max_rad /. (qa' -. (qb'2/.(4.0*.qc'))))) in
    let max_n = truncate (sqrt (max_rad /. (qc' -. (qb'2/.(4.0*.qa'))))) in
    let rectangle = product (range (-max_m) max_m) (range (-max_n) max_n) in
    Printf.printf "max_m: %d, max_n: %d, number of points in rectangle: %d\n" max_m max_n (List.length rectangle);
    (* returns list of possible v1s and list of possible v2s, as well as if a small vector was found *)
    let (v1_list, v2_list, very_small_vector_found) =
      let small_vector_threshold = sqrt (1./.100.) in
      let rec build' v1s v2s mns found =
        match mns with
        | [] -> (v1s, v2s, found)  (* these will be in reverse order, but who cares *)
        | (m, n) :: mns' ->
            if m = 0 && n = 0 then
              build' v1s v2s mns' found
            else
              let (f_m, f_n) = (float_of_int m, float_of_int n) in
              let v = (f_m*.b11 +. f_n*.b21, f_m*.b12 +. f_n*.b22) in
              let quadded = apply_quad q1 v in
              let (vx, vy) = v in
(*               Printf.printf "Lattice vector: (%.1f, %.1f). q1(v) = %f.\n" vx vy quadded; *)
              let found' = found || (quadded < small_vector_threshold) in
(*            Printf.printf "(%.1f, %.1f, %.1f, %.1f) applied to (%.1f, %.1f) is %f." qa qb qb' qc (fst v) (snd v) quadded; *)
              (* check that above inequalities are satisfied *)
              let v1s' =
                if quadded >= small_rad1 && quadded <= rad1 then (
(*                   Printf.printf "\t v1: yes"; *)
                  v::v1s
                ) else v1s
              in
              let v2s' =
                if quadded >= small_rad2 && quadded <= rad2 then (
(*                   Printf.printf "\t v2: yes"; *)
                  v::v2s
                ) else v2s
              in
(*               Printf.printf "\n"; *)
              build' v1s' v2s' mns' found'
      in
      build' [] [] rectangle false
    in
    (* builds list of possible gamma matrices *)
    let g_list =
      let rec build' gs v1v2s =
        match v1v2s with
        | [] -> gs
        | (v1, v2) :: v1v2s' ->
            let g = find_matrix b1 b2 v1 v2 in
            let (ga, gb, gc, gd) = g in
(*             Printf.printf "g: (%.1f %.1f %.1f %.1f) " ga gb gc gd; *)
            (* condition for g to be in SL2(ZZ) *)
            let gs' =
              if det g = 1.0 then (
(*                 Printf.printf "Accept."; *)
                g::gs
              ) else gs
            in
(*             Printf.printf "\n"; *)
            build' gs' v1v2s'
      in
      build' [] (product v1_list v2_list)
    in
    match g_list with
    | [] ->
        (max_float, true)
    | _ ->
        let un_nan x = if compare x Float.nan = 0 then 0.0 else x in
        let dist_after_gamma g : gamma * float =
          let q2' = multiply (transpose g) (multiply q2 g) in  (* we will find dist(q1, g^{-1}*q(2)) *)
          let bigA = multiply (inv q1) q2' in
          let (l1, l2) = eig bigA in
          let l1' = Float.abs (log l1) in  (* this distance between quad-forms is ... *)
          let l2' = Float.abs (log l2) in  (* ... equivalent to distance in H^2 *)
          let lmax = un_nan (if l1' > l2' then l1' else l2') in
          let (ga, gb, gc, gd) = g in
          let (x, y) = mobius g (x1, y1) in
          Printf.printf "g: (%.1f %.1f %.1f %.1f); Dist. b/w (%.1f, %.1f) and (%.1f, %.1f) is apparently %f.\n"
            ga gb gc gd x y x2 y2 lmax;
          (g, lmax)
        in
        let pairs = List.map dist_after_gamma g_list in
        let min' (g1, a) (g2, b) =
          let a', b' = un_nan a, un_nan b in
          if a' < b' then (g1, a') else (g2, b') in
        let (min_g, min_dist) = List.fold_left min' ((1.,0.,0.,1.), max_float) pairs in
        let (ga, gb, gc, gd) = min_g in
        Printf.printf
          "Dist. btw (%f, %f) and (%f, %f) is %f -- here gamma = (%f, %f, %f, %f). Small vector: %s.\n"
          x1 y1 x2 y2 min_dist ga gb gc gd (if very_small_vector_found then "true" else "false");
(* Printf.printf "Trace: %f\n" (ga +. gd); flush stdout; *)
        let halfplane_dist p q = fst (Halfplane.Halfplane.dist p q) in
        let ddd = halfplane_dist (mobius min_g (x1, y1)) (x2,y2) in
(*         Printf.printf "\tMobius transformation-calculated distance: %f.\n" ddd; *)
        (min_dist, not very_small_vector_found)

  let simpl (x,y) = (x,y)

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
    else raise (Sl2z_error "I am only trained to handle r=0.5.")

  let to_screen (x, y) r =
    (x, y *. cosh r), (y *. sinh r), (x, y)
    (* translate into "standard" fundamental domain before printing *)
(*
    let module C = Complex in
    let print_cplx z =
      Printf.printf "%f + i(%f)\n" z.C.re z.C.im
    in
    let z = { C.re = x; C.im = y } in
(*     Printf.printf "Simpl: "; print_cplx z; *)
    let rec translate_loop z' =
      if Float.abs (z'.C.re) <= 0.5 then
        z'
      else (
        if z'.C.re > 0.5 then ((*Printf.printf "T- ";*) translate_loop { C.re=z'.C.re -. 1.0; C.im=z'.C.im })
        else ((*Printf.printf "T ";*) translate_loop { C.re=z'.re +. 1.0; C.im=z'.im })
      )
    in
    let rec outer_loop z' =
      let z'' = translate_loop z' in
      if C.norm z'' >= 1.0 then z''
      else ((*Printf.printf "S- ";*) outer_loop (C.neg (C.inv z'')))
    in
    let z' = outer_loop z in
(*     Printf.printf "\nResult: "; print_cplx z'; *)
    let x' = z'.C.re in
    let y' = z'.C.im in
    (x', y' *. cosh r), (y' *. sinh r), (x', y')
*)
end
