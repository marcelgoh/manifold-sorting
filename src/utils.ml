(* Things that everyone will find useful *)

let tail_mapi f x_list =
  let rec helper idx acc x_list =
    match x_list with
    | [] -> List.rev acc
    | x :: xs -> helper (idx + 1) ((f idx x) :: acc) xs
  in
  helper 0 [] x_list
