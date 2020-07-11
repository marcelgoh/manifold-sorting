module type Grid = sig
  type point

  type grid

  val grid_size : grid -> int
  val to_list : grid -> point list
  val to_graph : grid -> float -> ((int * (float * float)) list * (int * int) list)

  (* both of these return a grid as well as a list of distance-comparison counts *)
  val fill_space : float -> point -> grid * (int * bool) list
  val fill_ball : point -> float -> float -> point -> grid * (int * bool) list
end
