module type Grid = sig
  type point

  type grid

  val grid_size : grid -> int
  val to_list : grid -> point list
  val build : float -> point -> grid
end
