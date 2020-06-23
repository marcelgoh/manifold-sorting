let torus u v =
  let (xu, yu), (xv, yv) = u, v in
  (module struct
     exception Zero_determinant

     type point = float * float

     let add_points (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)
     let scale_point alpha (x, y) = (alpha *. x, alpha *. y)

     let mat_vec_mul (a, b, c, d) (x, y) = (a *. x +. b *. y, c *. x +. d *. y)

     let e_dist (x1, y1) (x2, y2) = sqrt ((x1 -. x2) ** 2.0 +. (y1 -. y2) ** 2.0)

     (* takes a point and offsets it by xoffset, yoffset *)
     let offset (xoffset, yoffset) mult (px, py) =
       (px +. (mult *. xoffset), py +. (mult *. yoffset))

     (* compute quotient distance by copying query point into all neighbouring domains *)
     let dist p1 p2 =
       let nu = scale_point (-1.0) u in
       let nv = scale_point (-1.0) v in
       let candidates = [  (* the candidates in all neighbouring domains *)
           p1; add_points p1 u; add_points p1 v; add_points (add_points p1 v) u;
           add_points p1 nu; add_points p1 nv; add_points (add_points p1 nv) nu;
           add_points (add_points p1 nv) u; add_points (add_points p1 v) nu
         ] in
       let dists = List.map (fun c -> e_dist c p2) candidates in  (* nine distances *)
       let min a b = if a < b then a else b in
       List.fold_left min max_float dists  (* find minimum of distances *)
     (* get the representative within the parallelogram *)
     let simpl p =
       let det = xu *. yv -. xv *. yu in
       if det = 0.0 then raise Zero_determinant;
       let adjugate = (yv, (-. xv), (-. yu), xu) in
       let (x, y) = scale_point (1.0 /. det) (mat_vec_mul adjugate p) in
       let x_0 = x -. Float.floor x in  (* fractional part of x *)
       let y_0 = y -. Float.floor y in  (* fractional part of y *)
       mat_vec_mul (xu, xv, yu, yv) (x_0, y_0)  (* scale back up *)

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
       List.map (fun o -> offset o (r *. 2.0) p) offset_list

     let to_screen (x, y) r = (simpl (x, y), r)
   end : Space.Space with type point = float * float)
