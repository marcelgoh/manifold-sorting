module type Space = sig
  type point

  val dist : point -> point -> float
  val to_screen : point -> float * float
  val simpl : point -> point
  val get_local_cover : float -> point -> point list
end
