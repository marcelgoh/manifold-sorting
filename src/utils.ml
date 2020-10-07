(* Things that everyone will find useful *)

let tail_mapi f x_list =
  let rec helper idx acc x_list =
    match x_list with
    | [] -> List.rev acc
    | x :: xs -> helper (idx + 1) ((f idx x) :: acc) xs
  in
  helper 0 [] x_list

(* yes, using refs is sketchy, i agree. *)
let small_vector_denom = ref 20000.0
let halfplane_sl2z_r = ref 0.5
let halfplane_sl2z_use_lll = ref true
(* the N in Gamma_0(N) -- a value of 1 indicates SL2(ZZ) *)
let halfplane_sl2z_bigN = ref 1

let to_screen_disk (centre_x, centre_y) (u,v) r =
  (* convert a halfplane point to a disk point *)
  let h_to_d (x,y) =
    let x2 = x ** 2. in
    let denom = (x2 +. (1. +. y) ** 2.) in
    let disk_x = (2. *. x) /. denom in
    let disk_y = (x2 +. y ** 2. -. 1.) /. denom in
    (disk_x, disk_y)
  in
  (* calculate coordinates after shifting centrepoint to (0,1) *)
  let u' = (u -. centre_x) /. centre_y in
  let v' = v /. centre_y in
  let (p1x, p1y) = (u', exp(r)*.v') in
  let (p2x, p2y) = (u', exp(-.r)*.v') in
  let avg_y = (p1y +. p2y)/.2. in
  let (p3x, p3y) = (u'+. (p1y -. avg_y), avg_y) in
  let (x1, y1) = h_to_d (p1x, p1y) in
  let (x3, y3) = h_to_d (p2x, p2y) in  (* switched points 2 and 3, don't mind me *)
  let (x2, y2) = h_to_d (p3x, p3y) in
  let m_a = (y2-.y1) /. (x2-.x1) in
  let m_b = (y3-.y2) /. (x3-.x2) in
  let o_x = (m_a*.m_b*.(y1-.y3) +. m_b*.(x1+.x2) -. m_a*.(x2+.x3))/.(2.*.(m_b-.m_a)) in
  let o_y = (-.o_x +. (x1+.x2)/.2.)/.m_a +. (y1+.y2)/.2. in
  let eucl_rad = sqrt((x1 -. o_x)**2. +. (y1 -. o_y)**2.) in
  ((o_x, o_y), eucl_rad, h_to_d (u', v'))



