(* Metric space *)

module P = Printf
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
