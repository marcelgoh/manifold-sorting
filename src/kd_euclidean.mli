(* Kd implementation of Euclidean space *)

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

val grid_size : tree -> int

val to_list : tree -> float list list

(* the start point is assumed to be in the bounds *)
val fill_rect : float -> float -> float -> float -> float -> float list -> tree

val fill_para : float * float -> float * float -> float -> float list -> tree
