exception Lll_error

let set_vec x y =
  let _ = Array.mapi (fun i _ -> x.(i) <- y.(i)) x in ()

let add_vecf x y =
  Array.map2 (+.) x y

let sub_vecf x y =
  Array.map2 (-.) x y

let scale_vecf alpha x =
  Array.map (fun i -> alpha *. i) x

let add_veci x y =
  Array.map2 (+) x y

let sub_veci x y =
  Array.map2 (-) x y

let scale_veci alpha x =
  Array.map (fun i -> alpha * i) x

let dot x y =
  Array.fold_left (+.) 0. (Array.map2 ( *. ) x y)

let compute_gs k_max k bb_star bb mu b big_b =
  k_max := !k;
  set_vec bb_star.(!k) bb.(!k);
  for j = 1 to !k - 1 do
    mu.(!k).(j) <- b bb.(!k) bb_star.(j) /. big_b.(j);
    set_vec bb_star.(!k) (sub_vecf bb_star.(!k) (scale_vecf mu.(!k).(j) bb_star.(j)))
  done;
  big_b.(!k) <- b bb_star.(!k) bb_star.(!k);
  if abs_float big_b.(!k) < epsilon_float then
    raise Lll_error

let reduce mu k l bb h =
  if abs_float mu.(!k).(l) > 0.5 then
    let rounded = floor (0.5 +. mu.(!k).(l)) in
    set_vec bb.(!k) (sub_vecf bb.(!k) (scale_vecf rounded bb.(l)));
    set_vec h.(!k) (sub_veci h.(!k) (scale_veci (int_of_float rounded) h.(l)));
    mu.(!k).(l) <- mu.(!k).(l) -. rounded;
    for i = 1 to l - 1 do
      mu.(!k).(i) <- mu.(!k).(i) -. rounded *. mu.(l).(i)
    done

let lovasz big_b k mu =
  big_b.(!k) >= (0.75 -. mu.(!k).(!k - 1) *. mu.(!k).(!k - 1)) *. big_b.(!k - 1)

let arr_swap a i j =
  let x, y = a.(i), a.(j) in
  a.(j) <- x;
  a.(i) <- y

let swap bb k h mu big_b bb_star k_max =
  arr_swap bb !k (!k - 1);
  arr_swap h !k (!k - 1);
  if !k > 2 then
    for j = 1 to !k - 2 do
      let t = mu.(!k).(j) in
      mu.(!k).(j) <- mu.(!k - 1).(j);
      mu.(!k - 1).(j) <- t
    done;
  let m = mu.(!k).(!k - 1) in
  let t = big_b.(!k) +. m *. m *. big_b.(!k - 1) in
  mu.(!k).(!k - 1) <- m *. big_b.(!k - 1) /. t;
  let temp = Array.copy bb_star.(!k - 1) in
  set_vec bb_star.(!k - 1) (add_vecf bb_star.(!k) (scale_vecf m temp));
  set_vec bb_star.(!k) (add_vecf (scale_vecf (-.mu.(!k).(!k - 1)) bb_star.(!k))
                          (scale_vecf (big_b.(!k) /. t) temp));
  big_b.(!k) <- big_b.(!k - 1) *. big_b.(!k) /. t;
  big_b.(!k - 1) <- t;
  for i = !k + 1 to !k_max do
    let t = mu.(i).(!k) in
    mu.(i).(!k) <- mu.(i).(!k - 1) -. m *. t;
    mu.(i).(!k - 1) <- t +. mu.(!k).(!k - 1) *. mu.(i).(!k)
  done

let lll bb' b_matrix =
  let n = List.length bb' in
  let b_arr = Array.make_matrix (n + 1) (n + 1) 0. in
  let _ = List.mapi (fun i x -> List.mapi (fun j y -> b_arr.(i + 1).(j + 1) <- y) x) b_matrix in
  let b x y = dot (Array.map (fun row -> dot row x) b_arr) y in
  let q x = b x x in
  let k = ref 2 in
  let k_max = ref 1 in
  let bb = Array.make_matrix (n + 1) (n + 1) 0. in
  let _ = List.mapi (fun i x -> List.mapi (fun j y -> bb.(i + 1).(j + 1) <- y) x) bb' in
  let bb_star = Array.make_matrix (n + 1) (n + 1) 0. in
  Array.blit bb.(1) 1 bb_star.(1) 1 n;
  let h = Array.make_matrix (n + 1) (n + 1) 0 in
  let _ = Array.mapi (fun i hi -> hi.(i) <- 1) h in
  let mu = Array.make_matrix (n + 1) (n + 1) 0. in
  let big_b = Array.make (n + 1) 0. in
  big_b.(1) <- q bb_star.(1);
  let rec loop () =
    if !k > !k_max then compute_gs k_max k bb_star bb mu b big_b;
    let l = ref (!k - 1) in
    reduce mu k !l bb h;
    if lovasz big_b k mu then
      (
        l := !k - 2;
        let rec loop2 () =
          if !l > 0 then
            (
              reduce mu k !l bb h;
              l := !l - 1;
              loop2 ()
            );
        in
        loop2 ();
        k := !k + 1;
      )
    else
      (
        swap bb k h mu big_b bb_star k_max;
        k := max 2 (!k - 1);
      );
    if !k <= n then loop ()
  in
  loop ();
  let bb_list = List.tl (List.map (fun x -> List.tl (Array.to_list x)) (Array.to_list bb)) in
  let h_list = List.tl (List.map (fun x -> List.tl (Array.to_list x)) (Array.to_list h)) in
  bb_list, h_list
