

module Voronoi_eucl (Grid : Grid.Grid with type point := Euclidean.Euclidean.point) = struct
  type point = Euclidean.Euclidean.point
  type line_segment = point * point

  let dist p q = fst (Euclidean.Euclidean.dist p q)

  let all_pairs x_list =
    let rec helper acc x_list =
      match x_list with
      | [] -> acc
      | x :: xs -> helper ((List.map (fun l -> (x, l)) xs) @ acc) xs
    in
    helper [] x_list

  let circumcentre (ax, ay) (bx, by) (cx, cy) =
    let a' = ax ** 2.0 +. ay ** 2.0 in
    let b' = bx ** 2.0 +. by ** 2.0 in
    let c' = cx ** 2.0 +. cy ** 2.0 in
    let i_d = 1.0 /. (2.0 *. (ax *. (by -. cy) +. bx *. (cy -. ay) +. cx *. (ay -. by))) in
    let ux = i_d *. (a' *. (by -. cy) +. b' *. (cy -. ay) +. c' *. (ay -. by)) in
    let uy = i_d *. (a' *. (cx -. bx) +. b' *. (ax -. cx) +. c' *. (bx -. ax)) in
    (ux, uy)

  let get_vertices grid guarantee_radius : (int * int * int) list =
    let (nodes, edges) = Grid.to_graph grid (2.0 *. guarantee_radius) in
    let point_arr = Array.of_list (List.map snd nodes) in
    let edgelist_arr = Array.make (List.length nodes) [] in
    let handle_one_edge (i, j) =
      let j_edgelist = edgelist_arr.(j) in
      Array.set edgelist_arr j (i :: j_edgelist)
    in
    List.iter handle_one_edge edges;
    (* the list at index i in edgelist_arr now contains neighbours of vertex i *)
    let build_vertexlist i j_list =
      let j_pairs = all_pairs j_list in
      let f (p, q) =
        let cc = circumcentre point_arr.(i) point_arr.(p) point_arr.(q) in
        let dists = List.map (fun j -> dist point_arr.(j) cc) (List.filter (fun j -> j <> p && j <> q) j_list) in
        (dist point_arr.(i) cc) <= List.fold_left min guarantee_radius dists
      in
      List.map (fun (p,q) -> (i, p, q)) (List.filter f j_pairs)
    in
    (* to index i, associates a list of vertices of the Voronoi cell of i *)
    let vertexlist_arr = Array.mapi build_vertexlist edgelist_arr in
    Array.fold_left (@) [] vertexlist_arr

  let build (grid : Grid.grid) threshold : line_segment list =
    let all_vertex_pairs = all_pairs (get_vertices grid threshold) in
    let pts = Array.of_list (List.map snd (fst (Grid.to_graph grid (2. *. threshold)))) in
    let f ((x1, y1, z1), (x2, y2, z2)) = (y1 = y2 && z1 = z2) || (y1 = z2 && z1 = y2) in
    let filtered = List.filter f all_vertex_pairs in
    let circumcentres =
      List.map (fun ((a,b,c), (d,e,f)) -> (circumcentre pts.(a) pts.(b) pts.(c), circumcentre pts.(d) pts.(e) pts.(f))) filtered
    in
    circumcentres

  let covering_radius grid guarantee =
    let pts = Array.of_list (Grid.to_list grid) in
    let vertices = List.map (fun (a, b, c) -> circumcentre pts.(a) pts.(b) pts.(c)) (get_vertices grid guarantee) in
    let get_dist p =
      let neighbors = Grid.find_in_ball grid p guarantee in
      let dists = List.rev_map (dist p) neighbors in
      List.fold_left min max_float dists
    in
    let dists = List.map get_dist vertices in
    List.fold_left max min_float dists

end
