open Utils

let halfplane_soql q =
  (module struct
     exception Soql_error of string

     type point = float * float * float
     type loid_point = float * float * float
     type quad = loid_point -> float

     let mat_vec_mul (m1,m2,m3,m4,m5,m6,m7,m8,m9) (x1,x2,x3) =
       (m1*.x1 +. m2*.x2 +. m3*.x3, m4*.x1 +. m5*.x2 +. m6*.x3, m7*.x1 +. m8*.x2 +. m9*.x3)
     let det (a,b,c,d,e,f,g,h,i) =
       let det' (x,y,z,w) = x*.w -. y*.z in
       a*.(det' (e,f,h,i)) -. b*.(det' (d,f,g,i)) +. c*.(det' (d,e,g,h))
     let transpose (a,b,c,d,e,f,g,h,i) = (a,d,g,b,e,h,c,f,i)

     let make_quad d : quad = (fun (x,y,z) -> x ** 2. +. y ** 2. -. d ** z ** 2.)
     let to_string (px, py, pz) = Printf.sprintf "(%f, %f, %f)" px py pz
     let string_of_mat (m1,m2,m3,m4,m5,m6,m7,m8,m9) =
       Printf.sprintf "(%f, %f, %f,  %f, %f, %f,  %f, %f, %f)" m1 m2 m3 m4 m5 m6 m7 m8 m9
     let string_of_vec (x1,x2,x3) = Printf.sprintf "(%f, %f, %f)" x1 x2 x3

     (* returns distance if it is <= 0.5, otherwise returns max_float *)
     let dist p1 p2 : float * bool =
       let majorant q v x =
         let v' = e_unit3 v in
         let x_para = e_scalar_mul3 (e_dot3 v' x) v' in
         let x_perp = e_sub3 x x_para in
         q x_perp -. q x_para
       in
       let m1 = majorant q p1 in
       let m2 = majorant q p2 in
       let q2 x y = q (e_add3 x y) -. q x -. q y in

       let m1b x y = 0.5 *. (m1 (e_add3 x y) -. m1 x -. m1 y) in
       let e1, e2, e3 = (1., 0., 0.), (0., 1., 0.), (0., 0., 1.) in
       let m1_matrix = [[m1b e1 e1; m1b e2 e1 /. 2.; m1b e3 e1 /. 2.];
                        [m1b e1 e2 /. 2.; m1b e2 e2; m1b e3 e2 /. 2.];
                        [m1b e1 e3 /. 2.; m1b e2 e3 /. 2.; m1b e3 e3]] in
       let ([[b11; b12; b13]; [b21; b22; b23]; [b31; b32; b33]], _) =
         Lll.lll [[1.; 0.; 0.]; [0.; 1.; 0.]; [0.; 0.; 1.]] m1_matrix in
       let b1, b2, b3 = (b11, b12, b13), (b21, b22, b23), (b31, b32, b33) in

       let m2b x y = 0.5 *. (m2 (e_add3 x y) -. m2 x -. m2 y) in
       let a, b, c, e, f, i = m2b b1 b1, m2b b1 b2 *. 0.5, m2b b1 b3 *. 0.5, m2b b2 b2, m2b b2 b3 *. 0.5, m2b b3 b3 in
       let d = det (a, b, c, b, e, f, c, f, i) in
       let max1 = truncate (sqrt (exp 1. *. m1 b1 *. (e*.i -. f*.f) /. d)) in
       let max2 = truncate (sqrt (exp 1. *. m1 b2 *. (a*.i -. c*.c) /. d)) in
       let max3 = truncate (sqrt (exp 1. *. m1 b3 *. (a*.e -. b*.b) /. d)) in
       let get_f_dist ((a,d,g), (b,e,h), (c,f,i)) =
         let p1' = mat_vec_mul (a,b,c,d,e,f,g,h,i) p1 in
         let x = -. q2 p1' p2 in
         log (x +. sqrt(x ** 2. -. 1.))
       in
       let check_f ((a,d,g), (b,e,h), (c,f,i)) =
         let f' = mat_vec_mul (a,b,c,d,e,f,g,h,i) in
         q2 b1 b2 = q2 (f' b1) (f' b2) &&
           q2 b1 b3 = q2 (f' b1) (f' b3) &&
             q2 b2 b3 = q2 (f' b2) (f' b3) &&
               det (a,b,c,d,e,f,g,h,i) != 0.0
       in
       (* cohomology and arithmetic of locally symmetric spaces*)
       let range a b =
         let rec range' a b acc =
           if a = b then float_of_int b :: acc else range' a (b-1) (float_of_int b::acc)
         in
         range' a b []
       in
       let product l1 l2 l3 =
         List.flatten (List.flatten (List.map (fun a -> List.map (fun b -> List.map (fun c -> (a, b, c)) l3) l2) l1))
       in
       let fs = List.map (fun (a,b,c) -> e_scalar_mul3 a b1, e_scalar_mul3 b b2, e_scalar_mul3 c b3)
                  (product (range (-max1) max1) (range (-max2) max2) (range (-max3) max3)) in
       let d = List.fold_left min max_float (List.map get_f_dist (List.filter check_f fs)) in
       (d, true)
       (* we are trying to enumerate all f = gamma such that f(m1) is close to m2 *)
       (* LLL-reduce Z^3 lattice with respect to m1 to get basis b1,b2,b3 *)
       (* August 10 notes *)
       (* find all v1 satisfying some m2(v1) <= exp(2*r)*m1(b1) *)
       (* for every v1:
        * we know f maps b1 to v1
        * so the complement of span(b1) maps to the complement of span(v1)
        * LLL reduce span(b1)^\perp to get basis vectors b2', b3'
        * find all v2 satisfying m2(v2) <= exp(2*r)*m1(b2)
        * for every v2:
        * we know f maps b2' to v2
        * ... ?
        * find all v3 satisfying m2(v3) <= exp(2*r)*m1(b3)
        * for every v3:
        * we now know f. check if f is in SO(Q,L) -- August 24 notes (Q1 and Q2 near the end). *)
       (* loop over all f and take minimum *)

     let simpl (x,y,z) = (x,y,z)

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

     let get_local_cover r (x, y, z) =
       raise (Soql_error "Not implemented")

     let to_screen (x, y, z) r =
       raise (Soql_error "Not implemented")
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
   end : Space.Space with type point = float * float * float)
