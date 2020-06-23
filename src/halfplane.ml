module Halfplane : Space.Space with type point = float * float = struct
  exception Zero_determinant

  type point = float * float

  let dist p1 p2 = 1.

  let simpl p = p

  (* six offset functions: along with centre point, this covers a ball of radius 1
   * with balls of radius 1/2 *)
  let offset_list = [
      (0.0, 0.866);
      (0.75, 0.433);
      (0.75, -0.433);
      (0.0, -0.866);
      (-0.75, -0.433);
      (-0.75, 0.433);
    ]

  let get_local_cover r p =
    []

  let to_screen (x, y) r = (x, y *. sinh r), (y *. cosh r), (x, y)
end
