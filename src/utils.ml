(* Things that everyone will find useful *)

let tail_mapi f x_list =
  let rec helper idx acc x_list =
    match x_list with
    | [] -> List.rev acc
    | x :: xs -> helper (idx + 1) ((f idx x) :: acc) xs
  in
  helper 0 [] x_list

(* yes, using refs is sketchy, i agree. *)
let small_vector_denom = ref 20000.0
let halfplane_sl2z_r = ref 1.0
let halfplane_sl2z_use_lll = ref true;
