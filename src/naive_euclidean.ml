(* Naive implementation of Euclidean space *)

module D = DynArray
module S = Stack

exception Zero_determinant

type point = float * float
type grid = point D.t

let create_grid () : point D.t = D.create ()
let grid_size = D.length
let to_list g = List.map (fun (x,y) -> [x; y]) (D.to_list g)

let add_points (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)
let scale_point alpha (x, y) = (alpha *. x, alpha *. y)

let mat_vec_mul (a, b, c, d) (x, y) = (a *. x +. b *. y, c *. x +. d *. y)

let dist (x1, y1) (x2, y2) = sqrt ((x1 -. x2) ** 2.0 +. (y1 -. y2) ** 2.0)

(* compute quotient distance by copying query point into all neighbouring domains *)
let quotient_dist u v p1 p2 =
  let nu = scale_point (-1.0) u in
  let nv = scale_point (-1.0) v in
  let candidates = [  (* the candidates in all neighbouring domains *)
    p1; add_points p1 u; add_points p1 v; add_points (add_points p1 v) u;
    add_points p1 nu; add_points p1 nv; add_points (add_points p1 nv) nu
  ] in
  let dists = List.map (fun c -> dist c p2) candidates in  (* nine distances *)
  let min a b = if a < b then a else b in
  List.fold_left min max_float dists  (* find minimum of distances *)

(* get the representative within the parallelogram *)
let get_representative (xu, yu) (xv, yv) p =
  let det = xu *. yv -. xv *. yu in
  if det = 0.0 then raise Zero_determinant;
  let adjugate = (yv, (-. xv), (-. yu), xu) in
  let (x, y) = scale_point (1.0 /. det) (mat_vec_mul adjugate p) in
  let x_0 = x -. Float.floor x in  (* fractional part of x *)
  let y_0 = y -. Float.floor y in  (* fractional part of y *)
  mat_vec_mul (xu, xv, yu, yv) (x_0, y_0)  (* scale back up *)

let iter_grid = D.iter

(* takes a point and offsets it by xoffset, yoffset *)
let offset (xoffset, yoffset) mult (px, py) =
  (px +. (mult *. xoffset), py +. (mult *. yoffset))

(* six offset functions: along with centre point, this covers a ball of radius 1
 * with balls of radius 1/2 *)
let offset_list = [
  (0.0, 0.866);
  (0.75, 0.433);
  (0.75, -0.433);
  (0.0, -0.866);
  (-0.75, -0.433);
  (-0.75, 0.433);
]

let nearest_neighbour u v g p =
  if D.empty g then
    None
  else (
    let quot_dist = quotient_dist u v in
    (* this array stores pairs (dist_from_p, index) *)
    let dists : (float * int) D.t = D.make (D.length g) in
    (* iterate over the grid g to fill dists array *)
    D.iteri (fun i q -> D.add dists (quot_dist p q, i)) g;
    (* compares two dist pairs and keeps minimum *)
    let f (d1, i1) (d2, i2) = if d1 < d2 then (d1, i1) else (d2, i2) in
    (* get minimum over all dist pairs *)
    let (min_dist, i) = D.fold_left f (max_float, -1) dists in
    Some (D.get g i, min_dist)  (* return *)
  )

(* returns a boolean flag indicating whether point was added *)
let add_to_grid u v g threshold p =
  let rep = get_representative u v p in
  match nearest_neighbour u v g rep with
    None -> D.add g rep; true
  | Some (_, min_dist) ->
      (* let (px, py) = p in
      Printf.printf "(%f, %f) -- (%f, %f); min_dist = %f\n" px py qx qy min_dist; *)
      if min_dist > threshold then (D.add g rep; true) else false

(* fills a parallelogram; the start point is assumed to be in the bounds *)
let fill_para u v threshold start_p =
  let g = create_grid () in
  let stack = S.create () in
  S.push start_p stack;
  let rec loop () =
    if S.is_empty stack then
      g  (* return the grid *)
    else
      let p = S.pop stack in
      let offsets = List.map (fun o -> offset o (threshold *. 2.0) p) offset_list in
      if add_to_grid u v g threshold p then
        List.iter (fun q -> S.push q stack) offsets (* pushes 6 new points on stack *)
      ;
      loop ()
  in
  loop ()

