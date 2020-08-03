module type Space = sig
  type point

  (* distance from point to point and whether a local cover should be grown *)
  val dist : point -> point -> float * bool
  (*  (centre of circle, radius of circle, coordinate of point) *)
  val to_screen : point -> float -> (float * float) * float * (float * float)
  val simpl : point -> point
  val get_local_cover : float -> point -> point list
end
