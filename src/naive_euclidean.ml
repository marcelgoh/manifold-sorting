(* Naive implementation of Euclidean space *)

module D = DynArray

type point = float * float
type grid = point D.t

let threshold = 1.0

let create_grid () : point D.t = D.create ()

let dist (x1, y1) (x2, y2) = sqrt ((x1 -. x2) ** 2.0 +. (y1 -. y2) ** 2.0)

let iter_grid = D.iter

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

let add_to_grid g p =
  match nearest_neighbour g p with
    None -> D.add g p
  | Some (_, min_dist) ->
      if min_dist <= threshold then D.add g p
