module Halfplane_soql : Space.Space with type point = float * float = struct
  exception Soql_error of string

  type point = float * float
  type loid_point = float * float * float
  type quad = loid_point -> float

  let mat_vec_mul (m1,m2,m3,m4,m5,m6,m7,m8,m9) (x1,x2,x3) =
    (m1*.x1 +. m2*.x2 +. m3*.x3, m4*.x1 +. m5*.x2 +. m6*.x3, m7*.x1 +. m8*.x2 +. m9*.x3)

  let make_quad d : quad = (fun (x,y,z) -> x ** 2. +. y ** 2. -. d ** z ** 2.)
  let to_string (px, py) = Printf.sprintf "(%f, %f)" px py
  let string_of_mat (m1,m2,m3,m4,m5,m6,m7,m8,m9) =
    Printf.sprintf "(%f, %f, %f,  %f, %f, %f,  %f, %f, %f)" m1 m2 m3 m4 m5 m6 m7 m8 m9
  let string_of_vec (x1,x2,x3) = Printf.sprintf "(%f, %f, %f)" x1 x2 x3

  (* returns distance if it is <= 0.5, otherwise returns max_float *)
  let dist (x1, y1) (x2, y2) : float * bool =
    let denom1 = (x1 ** 2. +. (1. +. y1) ** 2.) in
    let disk_x1 = (2.0 *. x1) /. denom1 in
    let disk_y1 = (x1 ** 2. +. y1 ** 2. -. 1.) /. denom1 in
    let denom2 = (x2 ** 2. +. (1. +. y2) ** 2.) in
    let disk_x2 = (2.0 *. x2) /. denom2 in
    let disk_y2 = (x2 ** 2. +. y2 ** 2. -. 1.) /. denom2 in
    let denom3 = 1. -. disk_x1 ** 2. -. disk_y1 ** 2. in
    let t1 = (1. +. disk_x1 ** 2. +. disk_y1 ** 2.) /. denom3 in
    let x1' = (2. *. disk_x1) /. denom3 in
    let y1' = (2. *. disk_y1) /. denom3 in
    let denom4 = 1. -. disk_x2 ** 2. -. disk_y2 ** 2. in
    let t2 = (1. +. disk_x2 ** 2. +. disk_y2 ** 2.) /. denom4 in
    let x2' = (2. *. disk_x2) /. denom4 in
    let y2' = (2. *. disk_y2) /. denom4 in
    let p1 = (t1, x1', y1') in
    let p2 = (t2, x2', y2') in
    raise (Soql_error "Not yet implemented.")

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
    else raise (Soql_error "I am only trained to handle r=0.5.")

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
