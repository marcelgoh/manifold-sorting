module Halfplane_sl2z : Space.Space with type point = float * float = struct
  exception Sl2z_error of string

  type point = float * float
  type quad = float * float * float * float   (* matrix entries a b c d *)
  type gamma = float * float * float * float  (* group element gamma in SL_2(ZZ) *)

  let to_string (px, py) = Printf.sprintf "(%f, %f)" px py

  let modulo x y =
    let answer = x mod y in
    if answer >= 0 then answer else answer + (abs y)
  (* given m and n, compute gcd d and x and y such that mx + by = d (Knuth Vol. 1) *)
  let rec gcd m n =
    let rec loop a' a b' b c d =
      let r = modulo c d in
      let q = (c - r) / d in
(*       Printf.printf "%d = %d*%d + %d.\n" c q d r; *)
      if r = 0 then (a, b, d)
      else loop a (a' - q*a) b (b' - q*b) d r
    in
    loop 1 0 0 1 m n

  (* matrix acts on point by mobius transformation *)
  let mobius (a, b, c, d) (x,y) =
    let module C = Complex in
    let f rr : C.t = { re=rr; im=0.0 } in  (* complex_of_real *)
    let z : C.t = { re=x; im=y } in
    let (a', b', c', d') = (f a, f b, f c, f d) in
    let z' = C.div (C.add (C.mul a' z) b') (C.add (C.mul c' z) d') in
    (z'.re, z'.im)

  (* returns list of integers from a to b, inclusive -- as floats! *)
  let range a b =
    let rec range' a b acc =
      if a = b then float_of_int b :: acc else range' a (b-1) (float_of_int b::acc)
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
  let mat_mat_mul (a, b, c, d) (e,f,g,h) = (a*.e +. b*.g, a*.f +. b*.h, c*.e+.d*.g, c*.f+.d*.h)

  (* finds unique matrix that takes basis b1, b2 to basis v1, v2 *)
  let find_matrix (b1x, b1y) (b2x, b2y) (v1x, v1y) (v2x, v2y) =
    let det = (b1x *. b2y) -. (b2x *. b1y) in
    if det = 0. then raise (Sl2z_error "Zero determinant.");
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


  (* returns distance if it is <= !Utils.halfplane_sl2z_r, otherwise returns max_float *)
  let dist (x2, y2) (x1, y1) : float * bool =  (* THE ARGUMENTS ARE REVERSED! *)
    let small_vector_threshold = sqrt (1./. (!Utils.small_vector_denom)) in
    let verbose, lll =
      true, !Utils.halfplane_sl2z_use_lll
    in
    let q1 = quad_of_point (x1, y1) in
    let q2 = quad_of_point (x2, y2) in
    let (q1a, q1b, q1c, q1d) = q1 in let (q2a, q2b, q2c, q2d) = q2 in
    if verbose then Printf.printf "Quadratic form for point (%.1f, %.1f) is %fx^2 + %fxy + %fy^2.\n" x1 y1 q1a (q1b +. q1c) q1d;
    if verbose then Printf.printf "Quadratic form for point (%.1f, %.1f) is %fx^2 + %fxy + %fy^2.\n" x2 y2 q2a (q2b +. q2c) q2d;
    (* LLL reduction *)
    let (b11, b12, b21, b22) =
      if lll then (
        let (new_basis, bigH) = Lll.lll [[1.0; 0.0]; [0.0; 1.0]] [[q1a; q1b]; [q1c; q1d]] in
        (List.nth (List.nth new_basis 0) 0, List.nth (List.nth new_basis 0) 1,
         List.nth (List.nth new_basis 1) 0, List.nth (List.nth new_basis 1) 1)
    ) else (
        (1., 0., 0., 1.)
    ) in
    let (b1, b2) = ((b11, b12), (b21, b22)) in
    if verbose then Printf.printf "New basis: (%.2f, %.2f) (%.2f, %.2f).\n" b11 b12 b21 b22;
    (* compute bounds for v_i: e^(-1)q1(b1) <= q2(v1) <= e*q1(b1) and e^(-1)q1(b2) <= q2(v2) <= e*q1(b2) *)
    let (rad1, rad2, small_rad1, small_rad2) =
      let q1b1 = apply_quad q1 b1 in
      let q1b2 = apply_quad q1 b2 in
      let ee = exp (2. *. (!Utils.halfplane_sl2z_r)) in
      let small_ee = exp ((-.2.) *. (!Utils.halfplane_sl2z_r)) in
(*
      let ee = exp 1.0 in
      let small_ee = exp (-.1.0) in
*)
      (ee*.q1b1, ee*.q1b2, small_ee*.q1b1, small_ee*.q1b2)
    in
    if verbose then Printf.printf "rad1: %f, rad2: %f, small_rad1: %f, small_rad2: %f\n" rad1 rad2 small_rad1 small_rad2;
    (* LLL reduction *)
    let (b11, b12, b21, b22) =
      if lll then (
        let (new_basis, bigH) = Lll.lll [[1.0; 0.0]; [0.0; 1.0]] [[q2a; q2b]; [q2c; q2d]] in
        (List.nth (List.nth new_basis 0) 0, List.nth (List.nth new_basis 0) 1,
         List.nth (List.nth new_basis 1) 0, List.nth (List.nth new_basis 1) 1)
    ) else (
        (1., 0., 0., 1.)
    ) in
    let (qa, qb, qb'', qc) = q2 in
    if qb <> qb'' then raise (Sl2z_error "Quadratic form is not symmetric.");
    (* coefficients of quadratic form *)
    let qa' = qa *.b11*.b11 +. 2.0*.qb*.b11*.b12 +. qc*.b12*.b12 in
    let qb' = 2.0*.(qa*.b11*.b21 +. qb*.(b21*.b12 +. b11*.b22) +. qc*.b12*.b22) in
    let qc' = qa *.b21*.b21 +. 2.0*.qb*.b21*.b22 +. qc*.b22*.b22 in
    if verbose then Printf.printf "New quadratic form is %.10fx^2 + %.10fxy + %.10fy^2.\n" qa' qb' qc';
    let max_rad = if rad1 > rad2 then rad1 else rad2 in
    (* solve Lagrange multiplier problem *)
    let qb'2 = qb'*.qb' in
    if verbose then Printf.printf "max_rad: %.10f.\n" max_rad;
    let max_m = truncate (sqrt (max_rad /. (qa' -. (qb'2/.(4.0*.qc'))))) in
    let max_n = truncate (sqrt (max_rad /. (qc' -. (qb'2/.(4.0*.qa'))))) in
    (* the rectangle of possible coefficients *)
    let rectangle = product (range (-max_m) max_m) (range (-max_n) max_n) in
    if verbose then Printf.printf "max_m: %d, max_n: %d, number of points in rectangle: %d\n"
                                  max_m max_n (List.length rectangle);
    (* BUILD LIST OF POSSIBLE GAMMA MATRICES *)
    let ((b1x, b1y), (b2x, b2y)) = (b1, b2) in  (* the notation I used on paper :P *)
    let all_possible_v1s' = List.rev_map (fun (m,n) -> (m*.b1x +. n*.b2x, m*.b1y +. n*.b2y)) rectangle in
(*
    List.iter (fun (x,y) -> Printf.printf "(%f, %f)\n" x y) all_possible_v1s;
    Printf.printf "\n";
*)
    let all_possible_v1s = List.filter (fun (x,y) -> x <> 0. || y <> 0.) all_possible_v1s' in
let pairs = List.rev_map (fun v -> (v, apply_quad q2 v)) all_possible_v1s in
let ((mv1, mv2), min_quadded) = List.fold_left (fun (x,y) (x', y') -> if y < y' then (x,y) else (x', y')) (List.hd pairs) (List.tl pairs) in
if verbose then Printf.printf "Minimum vector: (%f, %f)\t q2-value: %f\n" mv1 mv2 min_quadded;
    let rec filter_v1s acc found v1s =
      match v1s with
      | [] -> (acc, found)
      | (0.0, 0.0) :: v1s' -> filter_v1s acc found v1s'
      | v1 :: v1s' ->
          let quadded = apply_quad q2 v1 in
          let (vx, vy) = v1 in
          if verbose then Printf.printf "v1: (%f, %f)  q2(v1) = %f\n" vx vy quadded;
          let found' = found || quadded < small_vector_threshold in
          let acc' = if quadded >= small_rad1 && quadded <= rad1 then v1 :: acc else acc in
          filter_v1s acc' found' v1s'
    in
    let (v1_list, very_small_vector_found) = filter_v1s [] false all_possible_v1s in
    let bigN = !Utils.halfplane_sl2z_bigN in  (* for short *)
    (* for every v1 vector, we build a list of possible (v1,v2) basis pairs
     * that will result in a gamma satisfying the mod bigN condition *)
    let matB = (b1x, b2x, b1y, b2y) in
    let inv_matB = inv matB in
    let handle_one_v1 v1 =
      let (vx, vy) = v1 in
      let (a, c) = mat_vec_mul inv_matB v1 in
      if verbose then Printf.printf "v1 = (%f, %f) results in a: %f\t c:%f\n" vx vy a c;
      if not (Float.is_integer a) || not (Float.is_integer c) then
        raise (Sl2z_error "Vector v1 is not integer combination of b1, b2.");
      let a', c' = int_of_float a, int_of_float c in
      let c2 =
        if a' = 0 then
          if abs c' = 1 then
            Some (float_of_int (-c'), 0.)
          else None
        else (
          if c' = 0 then
            if abs a' = 1 then
              Some (0., float_of_int a')
            else None
          else (
            let (c2y', c2x', d) =  gcd a' c' in  (* note they are switched *)
            if verbose then Printf.printf "%d = %d*%d + %d*%d\n" d c2y' a' c2x' c';
            if d <> 1 && d <> -1 then (
              None
            ) else (
              (* note we have to negate c2x *)
              if d = -1 then
                Some (float_of_int (c2x'), float_of_int (-c2y'))
              else
                Some (float_of_int (-c2x'), float_of_int c2y')
            )
          )
        )
      in
      match c2 with
      | None -> []
      | Some (c2x, c2y) ->
          let (v1x, v1y) = mat_vec_mul matB (a,c) in  (* standard basis so I don't get confused *)
          let (cx, cy) = mat_vec_mul matB (c2x, c2y) in
          let (bigA, bigB) = mat_vec_mul inv_matB (1.,0.) in
          let fa = bigB*.v1y in
          let fb = bigA*.v1y +. bigB*.cy in
          if not (Float.is_integer fa) || not (Float.is_integer fb) then raise (Sl2z_error "One of a,b not integer.");
          let a,b = int_of_float fa, int_of_float fb in
          let (_,_,d) = gcd a bigN in
          if b mod d <> 0 then
            []
          else (
            let a', b', bigN' = a/d, b/d, bigN/d in
            let (r'',s'',d'') = gcd a' bigN' in
            let (r', s', d') = if d = (-1) then (-r'',-s'',-d'') else (r'',s'',d'') in
            (* r'*a' + s'(bigN') = 1, so r'*a' == 1 (mod bigN) *)
            let t_cong = modulo (-(r'*b')) bigN' in  (* so t is congruent to t_cong (mod bigN') *)
            let (qa, qb, _, qc) = q2 in  (* just in case someone messes it up between here and above *)
            let alpha = qa*.v1x**2. +. 2.*.qb*.v1x*.v1y +. qc*.v1y**2. in
            let beta = qa*.v1x*.cx +. 2.*.qb*.(cx*.v1y +. v1x*.cy) +. qc*.v1y*.cy in
            let min_t = (-.beta)/.(2.*.alpha) in
            let temp = int_of_float (Float.floor (min_t /. float_of_int bigN')) in
            let flor' = temp - modulo temp bigN' + t_cong in
            let flor, ceel = if float_of_int flor' < min_t then flor', flor' + bigN' else flor' - bigN', flor' in
            let rec loop up acc t =
              let t' = float_of_int t in
              let v2_candidate = (cx +. t'*.v1x, cy +. t'*.v1y) in
              let (v21, v22) = v2_candidate in
              let (g1, g2, g3, g4) = find_matrix b1 b2 v1 v2_candidate in
              if verbose && (int_of_float g3 mod bigN) <> 0 then (
                Printf.printf "b1: (%f, %f)  b2: (%f, %f)  bigA: %f  bigB: %f\n" b1x b1y b2x b2y bigA bigB;
                Printf.printf "v1: (%f, %f)  c: (%f, %f)  a: %d  b: %d\n" v1x v1y cx cy a b;
                Printf.printf "a': %d  b': %d  bigN': %d  r': %d  s': %d  d': %d\n" a' b' bigN' r' s' d';
                Printf.printf "t_cong: %d  min_t: %f  flor: %d  ceel %d\n" t_cong min_t flor ceel;
                Printf.printf "here g: (%f, %f, %f, %f) from t = %d\n\n" g1 g2 g3 g4 t
              );
              let quadded = apply_quad q2 v2_candidate in
                if verbose then Printf.printf "v1: (%f, %f)\tv2_can: (%f, %f)\t quadded: %f\n" v1x v1y v21 v22 quadded;
              if quadded > rad2 then
                acc
              else (
                let acc' = if quadded >= small_rad2 then (v2_candidate :: acc) else acc in
                let t' = if up then t + bigN' else t - bigN' in
                loop up acc' t'
              )
            in
            let first_half = loop false [] flor in
            let v2s = loop true first_half ceel in
            List.rev_map (fun v2 -> (v1, v2)) v2s
          )
    in
    let rec tail_flatten acc lists =
      match lists with
      | [] -> acc
      | l :: ls -> tail_flatten (List.rev_append l acc) ls
    in
    let v1v2s = tail_flatten [] (List.rev_map handle_one_v1 v1_list) in
    let rec build_g_list acc v1v2s =
      match v1v2s with
      | [] -> acc
      | (v1, v2) :: v1v2s' ->
          let g = find_matrix b1 b2 v1 v2 in
          let (v11, v12), (v21, v22) = v1,v2 in
          if verbose then Printf.printf "v1: (%.1f, %.1f), v2: (%.1f, %.1f), \t q2(v2): %f\n" v11 v12 v21 v22 (apply_quad q2 v2); flush stdout;
          let (b1x, b1y), (b2x, b2y) = b1,b2 in
          let bigB = (b1x, b2x, b1y, b2y) in
          let (v11', v12'), (v21', v22') = mat_vec_mul (inv bigB) v1, mat_vec_mul (inv bigB) v2 in
          let (ga, gb, gc, gd) = g in
          if verbose then Printf.printf "\t a,c: (%.1f, %.1f), x,y: (%.1f, %.1f), \t g: (%.1f %.1f %.1f %.1f) " v11' v12' v21' v22' ga gb gc gd; flush stdout;
          let acc' =
            if det g = 1.0 then (
              if verbose then Printf.printf "Accept."; flush stdout;
              (g :: acc)
            ) else
              acc
          in
          if verbose then Printf.printf "\n"; flush stdout;
          build_g_list acc' v1v2s'
    in
    let g_list = build_g_list [] v1v2s in  (* GAMMA MATRICES WOOHOO *)
    if verbose then Printf.printf "g_length: %d\n" (List.length g_list);
    match g_list with
    | [] ->
        (max_float, not very_small_vector_found)
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
          if verbose then Printf.printf "(%.2f, %.2f) -- (%.2f, %.2f). g: (%.1f %.1f %.1f %.1f); dist((%.1f, %.1f), (%.1f, %.1f)) = %f.\n"
            x1 y1 x2 y2 ga gb gc gd x y x2 y2 lmax;
          (g, lmax)
        in
        let pairs = List.rev_map dist_after_gamma g_list in
        let min' (g1, a) (g2, b) =
          let a', b' = un_nan a, un_nan b in
          if a' < b' then (g1, a') else (g2, b') in
        let (min_g, min_dist) = List.fold_left min' ((1.,0.,0.,1.), max_float) pairs in
        let (ga, gb, gc, gd) = min_g in
(*         Printf.printf "b1: (%f, %f)\t b2: (%f, %f)\n" b1x b1y b2x b2y; *)
        if verbose then Printf.printf
          "Dist. btw (%f, %f) and (%f, %f) is %f -- here gamma = (%f, %f, %f, %f). Small vector: %s.\n"
          x1 y1 x2 y2 min_dist ga gb gc gd (if very_small_vector_found then "true" else "false");
(* Printf.printf "Trace: %f\n" (ga +. gd); flush stdout; *)
        let halfplane_dist p q = fst (Halfplane.Halfplane.dist p q) in
        let ddd = halfplane_dist (mobius min_g (x1, y1)) (x2,y2) in
(*         Printf.printf "\tMobius transformation-calculated distance: %f.\n" ddd; *)
        (min_dist, not very_small_vector_found)

  let simpl (x,y) = (x,y)

  let offset_list r' =
    let r = r' *. 2. in
    [
      (0.*.r, (0.4 -.1.) *.r +.1.);
      (0.*.r, (2.5 -.1.) *.r +.1.);
      (0.35*.r, (0.55 -.1.) *.r +.1.);
      (-0.35*.r, (0.55 -.1.) *.r +.1.);
      (0.8*.r, (0.8 -.1.) *.r +.1.);
      (-0.8*.r, (0.8 -.1.) *.r +.1.);
      (0.8*.r, (1.5 -.1.) *.r +.1.);
      (-0.8*.r, (1.5 -.1.) *.r +.1.);
    ]

  let get_local_cover r (x, y) =
    List.map (fun (x', y') -> (x +. (x' *. y), y *. y')) (offset_list r)

  let to_screen (x, y) r =
(*     (x, y *. cosh r), (y *. sinh r), (x, y) *)
    (* translate into "standard" fundamental domain before printing *)
    let module C = Complex in
    let print_cplx z =
      Printf.printf "%f + i(%f)\n" z.C.re z.C.im
    in
    let z = { C.re = x; C.im = y } in
(*     Printf.printf "simpl: "; print_cplx z; *)
    let rec translate_loop z' =
      if Float.abs (z'.C.re) <= 0.5 then
        z'
      else (
        if z'.C.re > 0.5 then ((*Printf.printf "t- ";*) translate_loop { C.re=z'.C.re -. 1.0; C.im=z'.C.im })
        else ((*Printf.printf "t ";*) translate_loop { C.re=z'.re +. 1.0; C.im=z'.im })
      )
    in
    let rec outer_loop z' =
      let z'' = translate_loop z' in
      if C.norm z'' >= 1.0 then z''
      else ((*Printf.printf "s- ";*) outer_loop (C.neg (C.inv z'')))
    in
    let z' = outer_loop z in
(*     Printf.printf "\nresult: "; print_cplx z'; *)
    let x' = z'.C.re in
    let y' = z'.C.im in
    (x', y' *. cosh r), (y' *. sinh r), (x', y')
end
