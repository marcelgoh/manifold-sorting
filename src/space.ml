module type Space = sig
  type point

  val dist : point -> point -> float
  val to_screen : point -> float list
  val simpl : point -> point
  val get_local_cover : float -> point -> point list
end
