module S = Queue

module Kd (Space : Space.Space) (F : sig val to_e : Space.point -> float -> float list * (float * float) list end) : Grid.Grid with type point := Space.point =
  struct
    type node_info = {
        axis : int;
        pos :  float;
        p : float list;
        p' : Space.point;
        n : int;
      }

    exception Dimension_mismatch of int * int

    type tree =
      | Empty
      | Node of node_info * tree * tree

    type grid = tree

    let rec grid_size g =
      match g with
      | Empty -> 0
      | Node (_, left, right)  -> 1 + grid_size left + grid_size right

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
      let simpl_p = Space.simpl p in
      let p', rect = F.to_e simpl_p threshold in
      let dists = List.map (fun q -> Space.dist q simpl_p) (find_in_range g rect) in
      (* Printf.printf "(%f, %f) (%f, %f)\n" (fst (List.nth rect 0)) (snd (List.nth rect 0)) (fst (List.nth rect 1)) (snd (List.nth rect 1)); flush stdout; *)
      match List.find_opt (fun d -> d < threshold) dists with
      | None ->
         (* Printf.printf "%f\n" ((float_of_int (List.length dists)) /. float_of_int (grid_size g)); flush stdout; *)
         (insert g p' n simpl_p, true, List.length dists)
      | Some x -> (g, false, List.length dists)

    let fill_space threshold start_p =
      let g = Empty in
      let stack = S.create () in
      S.push start_p stack;
      let rec loop grid i comp_counts =
        if S.is_empty stack then
          (grid, List.rev comp_counts)
        else
          let p = S.pop stack in
          let (newgrid, added, count) = add_to_grid grid threshold p i in
          if added then (
            List.iter (fun q -> S.push q stack) (Space.get_local_cover threshold p); (* pushes 6 new points on stack *)
            loop newgrid (i + 1) ((count, true) :: comp_counts) (* reset accumulator *)
          ) else
            (* we still need to add the number of counts even though we're not recording yet *)
            loop newgrid i ((count, false) :: comp_counts)
      in
      loop g 0 []

    (* returns the grid as well as a list of comparison counts *)
    let fill_ball center r threshold start_p =
      let g = Empty in
      let stack = S.create () in
      S.push start_p stack;
      let rec loop grid i comp_counts =
        if S.is_empty stack then
          (grid, List.rev comp_counts)
        else
          let p = S.pop stack in
          let (newgrid, added, count) = add_to_grid grid threshold p i in
          (* Printf.printf "%d\n" (grid_size grid); flush stdout; *)
          if added then (
            List.iter (fun q -> S.push q stack) (List.filter (fun x -> r > Space.dist x center) (Space.get_local_cover threshold p)); (* pushes 6 new points on stack *)
            loop newgrid (i + 1) ((count, true) :: comp_counts)
          ) else
            loop newgrid i ((count, false) :: comp_counts)
      in
      loop g 0 []
  end
