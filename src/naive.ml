module D = DynArray
module S = Queue

module Naive (Space : Space.Space) = (struct
  exception Naive_error
  type grid = Space.point D.t

  let create_grid () : Space.point D.t = D.create ()
  let grid_size = D.length
  let iter_grid = D.iter

  let to_list g = D.to_list g

  let to_graph g threshold = raise Naive_error
  let find_in_ball g p r = raise Naive_error

  let nearest_neighbour g p =
    if D.empty g then
      None
    else (
      (* this array stores pairs (dist_from_p, index) *)
      let dists : ((float * bool) * int) D.t = D.make (D.length g) in
      (* iterate over the grid g to fill dists array *)
      D.iteri (fun i q -> D.add dists (Space.dist p q, i)) g;
      (* compares two dist pairs and keeps minimum *)
      let f ((d1, b1), i1) ((d2, b2), i2) = if d1 < d2 then ((d1, b1), i1) else ((d2, b2), i2) in
      (* get minimum over all dist pairs *)
      let ((min_dist, grow_local_cover), i) = D.fold_left f ((max_float, true), -1) dists in
      Some (D.get g i, min_dist, D.length dists, grow_local_cover)  (* return *)
    )

  (* returns a boolean flag indicating whether point was added, as well as a comparison count *)
  let add_to_grid g threshold p : bool * int * bool =
    let rep = Space.simpl p in
    match nearest_neighbour g rep with
    | None -> D.add g rep; (true, 0, true)
    | Some (q, min_dist, comp_count, glc) ->
       let ((px, py), _, _) = Space.to_screen p 0.0 in
       let ((qx, qy), _, _) = Space.to_screen q 0.0 in
       Printf.printf "(%f, %f) -- (%f, %f); min_dist = %f\n" px py qx qy min_dist;
       if min_dist > threshold then (Printf.printf "(%f, %f) was added!\n" px py; D.add g rep; (true, comp_count, glc)) else (false, comp_count, glc)

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
        let (flag, count, grow_local_cover) = add_to_grid g threshold p in
        if flag && grow_local_cover then (
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
        let offsets = List.filter (fun x -> r >= fst (Space.dist x center)) (Space.get_local_cover threshold p) in
        let (flag, count, grow_local_cover) = add_to_grid g threshold p in
        if flag && grow_local_cover then (
            List.iter (fun q -> S.push q stack) offsets; (* pushes 6 new points on stack *)
            loop ((count, true) :: comp_counts)
        ) else
          (* else *)
          (* Printf.printf "not added\n"; *)
          loop ((count, false) :: comp_counts)
    in
    loop []
end : Grid.Grid with type point := Space.point)
