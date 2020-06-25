module type Grid = sig
  type point

  type grid

  val grid_size : grid -> int
  val to_list : grid -> point list
  val fill_space : float -> point -> grid
  val fill_ball : point -> float -> float -> point -> grid
end
