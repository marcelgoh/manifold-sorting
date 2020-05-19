(* Metric space *)

module D = DynArray

module type Metric = sig
  type point
  type grid
  val dist : point -> point -> float
  (* returns nearest neighbour in the grid as well as the distance *)
  val nearest_neighbour : grid -> point -> (point * float) option
  (* adds a point to grid if it is not too close to any other point *)
  val add_to_grid : grid -> point -> unit
end

module Euclidean2 : Metric = struct
  type point = float * float
  type grid = point D.t
  let threshold = 1.0

  let dist (x1, y1) (x2, y2) = sqrt ((x1 -. x2) ** 2.0 +. (y1 -. y2) ** 2.0)

  let nearest_neighbour g p =
    if D.empty g then
      None
    else
      let dists : (float * int) D.t = D.make (D.length g) in
      for i = 0 to D.length g - 1 do
        D.set dists i (dist p (D.get g i), i)
      done;
      let f (d1, i1) (d2, i2) = if d1 < d2 then (d1, i1) else (d2, i2) in
      let (min_dist, i) = D.fold_left f (max_float, -1) dists in
      Some (D.get g i, min_dist)

  let add_to_grid g p =
    match nearest_neighbour g p with
      None -> D.add g p
    | Some (_, min_dist) ->
        if min_dist <= threshold then D.add g p
end
