module D = DynArray
module S = Queue

module Naive (Space : Space.Space) = (struct
  type grid = Space.point D.t

  let create_grid () : Space.point D.t = D.create ()
  let grid_size = D.length
  let iter_grid = D.iter

  let to_list g = D.to_list g

  let nearest_neighbour g p =
    if D.empty g then
      None
    else (
      (* this array stores pairs (dist_from_p, index) *)
      let dists : (float * int) D.t = D.make (D.length g) in
      (* iterate over the grid g to fill dists array *)
      D.iteri (fun i q -> D.add dists (Space.dist p q, i)) g;
      (* compares two dist pairs and keeps minimum *)
      let f (d1, i1) (d2, i2) = if d1 < d2 then (d1, i1) else (d2, i2) in
      (* get minimum over all dist pairs *)
      let (min_dist, i) = D.fold_left f (max_float, -1) dists in
      Some (D.get g i, min_dist, D.length dists)  (* return *)
    )

  (* returns a boolean flag indicating whether point was added, as well as a comparison count *)
  let add_to_grid g threshold p : bool * int =
    let rep = Space.simpl p in
    match nearest_neighbour g rep with
    | None -> D.add g rep; (true, 0)
    | Some (_, min_dist, comp_count) ->
       (* let (px, py) = p in
      Printf.printf "(%f, %f) -- (%f, %f); min_dist = %f\n" px py qx qy min_dist; *)
       if min_dist > threshold then (D.add g rep; (true, comp_count)) else (false, comp_count)

  (* fills a parallelogram; the start point is assumed to be in the bounds *)
  let fill_space threshold start_p =
    let g = create_grid () in
    let stack = S.create () in
    S.push start_p stack;
    let rec loop comp_counts =
      if S.is_empty stack then
        (g, List.rev comp_counts)  (* return the grid *)
      else
        let p = S.pop stack in
        (* Printf.printf "%f, %f " (fst p) (snd p); *)
        let offsets = Space.get_local_cover threshold p in
        let (flag, count) = add_to_grid g threshold p in
        if flag then (
            (* Printf.printf "added\n"; *)
            List.iter (fun q -> S.push q stack) offsets; (* pushes 6 new points on stack *)
            loop ((count, true) :: comp_counts)
        ) else
          loop ((count, false) :: comp_counts)
    in
    loop []

  let fill_ball center r threshold start_p =
    let g = create_grid () in
    let stack = S.create () in
    S.push start_p stack;
    let rec loop comp_counts =
      if S.is_empty stack then
        (g, List.rev comp_counts)  (* return the grid *)
      else
        let p = S.pop stack in
        (* Printf.printf "%f, %f " (fst p) (snd p); *)
        let offsets = List.filter (fun x -> r >= Space.dist x center) (Space.get_local_cover threshold p) in
        let (flag, count) = add_to_grid g threshold p in
        if flag then (
            (* Printf.printf "added\n"; *)
            List.iter (fun q -> S.push q stack) offsets; (* pushes 6 new points on stack *)
            loop ((count, true) :: comp_counts)
        ) else
          (* else *)
          (* Printf.printf "not added\n"; *)
          loop ((count, false) :: comp_counts)
    in
    loop []
end : Grid.Grid with type point := Space.point)
