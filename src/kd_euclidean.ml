(* Kd implementation of Euclidean space *)

module D = DynArray
module S = Stack

type point = float list

type node_info = {
  axis : int;
  pos :  float;
  p : point;
  p' : point;
  n : int;
}

type tree =
  | Empty
  | Node of node_info * tree * tree

let pi = (acos (-.1.))

let rec grid_size g =
  match g with
  | Empty -> 0
  | Node (_, left, right)  -> 1 + grid_size left + grid_size right

let to_list g =
  let rec tolist_l g l =
    match g with
    | Empty -> l
    | Node (info, left, right) -> (info.p, info.n) :: tolist_l left (tolist_l right l)
  in
  let compare (_, m) (_, n) = m - n in
  let pair_list = tolist_l g [] in
  let sorted = List.sort compare pair_list in
  List.map fst sorted

let dist p1 p2 =
  let rec sumdiffsq x1 x2 =
    match x1, x2 with
    | [], [] -> 0.
    | a1 :: q1, a2 :: q2 -> (a1 -. a2) ** 2. +. sumdiffsq q1 q2
  in sqrt (sumdiffsq p1 p2)

let para_dist (a, c) (b, d) [x1; y1] [x2; y2] =
  let det = (a *. d) -. (c *. b) in
  let a', c', b', d' = d /. det, -.b /. det, -.c /. det, a /. det in
  let x1', y1' = (a' *. x1) +. (b' *. y1), (c' *. x1) +. (d' *. y1) in
  let x2', y2' = (a' *. x2) +. (b' *. y2), (c' *. x2) +. (d' *. y2) in
  dist [x1' -. Float.floor x1'; y1' -. Float.floor y1'] [x2' -. Float.floor x2'; y2' -. Float.floor y2']

(* takes a point and offsets it by xoffset, yoffset *)
let rec offset o mult p =
  match o, p with
  | [], [] -> []
  | a :: otail, b :: ptail -> b +. (mult *. a) :: offset otail mult ptail

(* six offset functions: along with centre point, this covers a ball of radius 1
 * with balls of radius 1/2 *)
let offset_list = [
  [0.0; 0.866];
  [0.75; 0.433];
  [0.75; -0.433];
  [0.0; -0.866];
  [-0.75; -0.433];
  [-0.75; 0.433];
]

let rec nearest_neighbour g p =
  match g with
  | Empty -> None
  | Node (info, left, right) ->
     let l =
       if List.nth p info.axis -. info.pos < 1. then
         nearest_neighbour left p
       else None
     in
     let r =
       if List.nth p info.axis -. info.pos > -1. then
         nearest_neighbour right p
       else None
     in
     let q = info.p in
     match l, r with
     | None, None -> Some q
     | Some x, None -> Some (if dist p x < dist p q then x else q)
     | None, Some y -> Some (if dist p y < dist p q then y else q)
     | Some x, Some y -> Some (
       if dist p x < dist p y then
         if dist p x < dist p q then x
         else q
       else
         if dist p y < dist p q then y
         else q
       )

let rec find_in_range g rect =
  let in_rect x rect =
    (List.find_opt (fun (x, (min, max)) -> min > x || x > max) (List.combine x rect)) == None
  in
  match g with
  | Empty -> []
  | Node (info, left, right) ->
     (if in_rect info.p rect then [info.p'] else [])
     @ (if info.pos >= fst (List.nth rect info.axis) then find_in_range left rect else [])
     @ (if info.pos <= snd (List.nth rect info.axis) then find_in_range right rect else [])

let insert g p n p' =
  let rec insertaxis g p newaxis d n p' =
    match g with
    | Empty ->
       let info = { axis = newaxis; pos = List.nth p newaxis; p; n; p' } in
       Node (info, Empty, Empty)
    | Node (info, left, right) ->
       let next = (info.axis + 1) mod d in
       if List.nth p info.axis < info.pos
       then Node (info, insertaxis left p next d info.n p', right)
       else Node (info, left, insertaxis right p next d info.n p') in
  let newaxis =
    match g with
     | Empty -> 0
     | Node (info, _, _) -> info.axis
  in
  insertaxis g p newaxis (List.length p) n p'

let add_to_grid g threshold p n =
  let rect = (List.map (fun x -> (x -. threshold, x +. threshold)) p) in
  let dists = List.map (fun q -> dist p q) (find_in_range g rect) in
  let min_dist = List.fold_left min max_float dists in
  if min_dist > threshold then (insert g p n p, true) else (g, false)

let para_to_euclidean (a, c) (b, d) [x; y] =
  let det = (a *. d) -. (c *. b) in
  let a', c', b', d' = d /. det, -.b /. det, -.c /. det, a /. det in
  let x', y' = (a' *. x) +. (b' *. y), (c' *. x) +. (d' *. y) in
  [cos (2. *. pi *. x'); cos (2. *. pi *. y')]

let add_to_grid_para u v g threshold p n =
  let p' = para_to_euclidean u v p in
  let rect = (List.map (fun x -> (x -. threshold, x +. threshold)) p') in
  let dists = List.map (fun q -> para_dist u v p q) (find_in_range g rect) in
  match List.find_opt (fun d -> d < threshold) dists with
  | None -> (insert g p' n p, true)
  | Some x ->  (g, false)

(* the start point is assumed to be in the bounds *)
let fill_rect left right top bottom threshold start_p =
  let i = ref 1 in
  let g = ref Empty in
  let stack = S.create () in
  S.push start_p stack;
  let rec loop () =
    incr i;
    if S.is_empty stack then
      !g  (* return the grid *)
    else
      let p = S.pop stack in
      let offsets = List.map (fun o -> offset o (threshold *. 2.0) p) offset_list in
      let f q =
        let qx = List.nth q 0 in
        let qy = List.nth q 1 in
        if qx >= left && qx <= right && qy <= top && qy >= bottom then
          S.push q stack;  (* push new point if within bounds *)
        incr i
      in
      let (newgrid, added) = add_to_grid !g threshold p !i in
      g := newgrid;
      if added then
        List.iter f offsets (* pushes 6 new points on stack *)
      else ();
      loop ()
  in
  loop ()

let fill_para u v threshold start_p =
  let i = ref 1 in
  let g = Empty in
  let stack = S.create () in
  S.push start_p stack;
  let rec loop grid =
    if S.is_empty stack then
      grid
    else
      let p = S.pop stack in
      let (newgrid, added) = add_to_grid_para u v grid threshold p !i in
      if added then
        let offsets = List.map (fun o -> offset o (threshold *. 2.0) p) offset_list in
        List.iter (fun q -> S.push q stack) offsets (* pushes 6 new points on stack *)
      else ();
      loop newgrid
  in
  loop g
