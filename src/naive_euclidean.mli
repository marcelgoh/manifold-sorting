(* Naive implementation of Euclidean space *)

module D = DynArray
module S = Stack

exception Zero_determinant

type point = float * float
type grid = point D.t

val grid_size : grid -> int
val to_list : grid -> float list list

val quotient_dist : float * float -> float * float -> float * float -> float * float -> float

(* fills a parallelogram; the start point is assumed to be in the bounds *)
val fill_para : float * float -> float * float -> float -> float * float -> grid
