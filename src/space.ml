module type Space = sig
  type point

  val dist : point -> point -> float
  (*  (centre of circle, radius of circle, coordinate of point) *)
  val to_screen : point -> float -> (float * float) * float * (float * float)
  val simpl : point -> point
  val get_local_cover : float -> point -> point list
end
