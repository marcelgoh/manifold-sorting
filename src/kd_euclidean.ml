(* Kd implementation of Euclidean space *)

module D = DynArray
module S = Queue
module N = Naive_euclidean

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

exception Dimension_mismatch of int * int

let pi = (acos (-.1.))

let rec grid_size g =
  match g with
  | Empty -> 0
  | Node (_, left, right)  -> 1 + grid_size left + grid_size right

let get_representative (a, c) (b, d) p =
  match p with
  | [x; y] ->
     let (x', y') = N.get_representative (a, c) (b, d) (x, y) in [x'; y']
  | _ -> raise (Dimension_mismatch (1, List.length p))
  (* let det = (a *. d) -. (c *. b) in
   * let a', c', b', d' = d /. det, -.b /. det, -.c /. det, a /. det in
   * let x', y' = (a' *. x) +. (b' *. y), (c' *. x) +. (d' *. y) in
   * let x'', y'' = x' -. Float.floor x', y' -. Float.floor y' in
   * [a *. x'' +. b *. y''; c *. x'' +. d *. y''] *)

let to_list g =
  let rec tolist_l g l =
    match g with
    | Empty -> l
    | Node (info, left, right) -> tolist_l left ((info.p', info.n) :: tolist_l right l)
  in
  let cpare (_, m) (_, n) = m - n in
  let pair_list = tolist_l g [] in
  let sorted = List.stable_sort cpare pair_list in
  List.map fst sorted

let dist p1 p2 =
  let rec sumdiffsq x1 x2 =
    match x1, x2 with
    | [], [] -> 0.
    | a1 :: q1, a2 :: q2 -> (a1 -. a2) ** 2. +. sumdiffsq q1 q2
    | _ -> raise (Dimension_mismatch (List.length x1, List.length x2))
  in sqrt (sumdiffsq p1 p2)

(* takes a point and offsets it by xoffset, yoffset *)
let rec offset o mult p =
  match o, p with
  | [], [] -> []
  | a :: otail, b :: ptail -> b +. (mult *. a) :: offset otail mult ptail
  | _ -> raise (Dimension_mismatch (List.length o, List.length p))

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
       then Node (info, insertaxis left p next d n p', right)
       else Node (info, left, insertaxis right p next d n p') in
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

let para_to_euclidean (a, c) (b, d) p =
  match p with
  | [x; y] ->
     let det = (a *. d) -. (c *. b) in
     let a', c', b', d' = d /. det, -.c /. det, -.b /. det, a /. det in
     let x', y' = (a' *. x) +. (b' *. y), (c' *. x) +. (d' *. y) in
     let l = (sqrt ((a ** 2.) +. (b ** 2.))) /. (2. *. pi) in
     [cos (2. *. pi *. x') *. l; cos (2. *. pi *. y') *. l]
  | _ -> raise (Dimension_mismatch (2, List.length p))

let add_to_grid_para u v g threshold p n =
  let rep_p = get_representative u v p in
  let p' = para_to_euclidean u v rep_p in
  let rect = (List.map (fun x -> (x -. threshold, x +. threshold)) p') in
  let dists = List.map (fun q ->
                  match q, rep_p with
                  | [qx; qy], [px; py] -> N.quotient_dist u v (px, py) (qx, qy)
                  | _ -> raise (Dimension_mismatch (2, if List.length p == 2 then List.length q else List.length p))) (find_in_range g rect) in
  match List.find_opt (fun d -> d < threshold) dists with
  | None -> (insert g p' n rep_p, true)
  | Some x -> (g, false)

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
  let g = Empty in
  let stack = S.create () in
  S.push start_p stack;
  let rec loop grid i =
    if S.is_empty stack then
      grid
    else
      let p = S.pop stack in
      (* let repr = get_representative u v p in *)
      (* Printf.printf "(%f, %f) %f, %f " (List.nth repr 0) (List.nth repr 1) (List.nth p 0) (List.nth p 1); *)
      let (newgrid, added) = add_to_grid_para u v grid threshold p i in
      (* Printf.printf "%sadded\n" (if added then "" else "not "); *)
      if added then
        let offsets = List.map (fun o -> offset o (threshold *. 2.0) p) offset_list in
        List.iter (fun q -> S.push q stack) offsets; (* pushes 6 new points on stack *)
        loop newgrid (i + 1)
      else
        loop newgrid i
  in
  loop g 0
