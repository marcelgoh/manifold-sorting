module Halfplane : Space.Space with type point = float * float = struct
  exception Hyperbolic_error

  type point = float * float

  let dist (x1, y1) (x2, y2) =
    let a = (x2 -. x1) ** 2. in
    let b = (y2 -. y1) ** 2. in
    let c = (y2 +. y1) ** 2. in
    (2. *. log ( ((sqrt (a +. b)) +. (sqrt (a +. c))) /. (2. *. sqrt (y1 *. y2)) ), true)

  let simpl p = p

  let offset_list =
    [
      (0., 0.4);
      (0., 2.5);
      (0.35, 0.55);
      (-0.35, 0.55);
      (0.8, 0.8);
      (-0.8, 0.8);
      (0.8, 1.5);
      (-0.8, 1.5);
    ]

  let get_local_cover r (x, y) =
    if r = 0.5 then
      List.map (fun (x', y') -> (x +. (x' *. y), y *. y')) offset_list
    else raise Hyperbolic_error

  let to_screen (x, y) r = (x, y *. cosh r), (y *. sinh r), (x, y)
end
