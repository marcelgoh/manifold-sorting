module Euclidean : Space.Space with type point = float * float = struct
  type point = float * float

  let dist (x1, y1) (x2, y2) = (sqrt ((x1 -. x2) ** 2.0 +. (y1 -. y2) ** 2.0), true)
  let to_screen p r = (p, r, p)
  let simpl p = p
  let offset_list = [
      (0.0, 0.866);
      (0.75, 0.433);
      (0.75, -0.433);
      (0.0, -0.866);
      (-0.75, -0.433);
      (-0.75, 0.433);
    ]

  let offset (xoffset, yoffset) mult (px, py) =
    (px +. (mult *. xoffset), py +. (mult *. yoffset))
  let get_local_cover r p =
    List.map (fun o -> offset o (r *. 2.0) p) offset_list
end
