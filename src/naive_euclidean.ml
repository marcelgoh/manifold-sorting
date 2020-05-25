(* Naive implementation of Euclidean space *)

module D = DynArray
module S = Stack

type point = float * float
type grid = point D.t

let create_grid () : point D.t = D.create ()
let grid_size g = D.length g

let dist (x1, y1) (x2, y2) = sqrt ((x1 -. x2) ** 2.0 +. (y1 -. y2) ** 2.0)

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

let nearest_neighbour g p =
  if D.empty g then
    None
  else (
    if D.length g = 0 then Printf.printf "How can this happen?\n";
    (* this array stores pairs (dist_from_p, index) *)
    let dists : (float * int) D.t = D.make (D.length g) in
    (* iterate over the grid g to fill dists array *)
    D.iteri (fun i q -> D.add dists (dist p q, i)) g;
    (* compares two dist pairs and keeps minimum *)
    let f (d1, i1) (d2, i2) = if d1 < d2 then (d1, i1) else (d2, i2) in
    (* get minimum over all dist pairs *)
    let (min_dist, i) = D.fold_left f (max_float, -1) dists in
    Some (D.get g i, min_dist)  (* return *)
  )

(* returns a boolean flag indicating whether point was added *)
let add_to_grid g threshold p =
  match nearest_neighbour g p with
    None -> D.add g p; true
  | Some ((qx, qy), min_dist) ->
      (* let (px, py) = p in
      Printf.printf "(%f, %f) -- (%f, %f); min_dist = %f\n" px py qx qy min_dist; *)
      if min_dist > threshold then (D.add g p; true) else false

(* the start point is assumed to be in the bounds *)
let fill_rect left right top bottom threshold start_p =
  let g = create_grid () in
  let stack = S.create () in
  S.push start_p stack;
  let rec loop () =
    if S.is_empty stack then
      g  (* return the grid *)
    else
      let p = S.pop stack in
      let offsets = List.map (fun o -> offset o (threshold *. 2.0) p) offset_list in
      let f (qx, qy) =
        if qx >= left && qx <= right && qy <= top && qy >= bottom then
          S.push (qx, qy) stack  (* push new point if within bounds *)
      in
      if add_to_grid g threshold p then
        List.iter f offsets (* pushes 6 new points on stack *)
      else ();
      loop ()
  in
  loop ()



