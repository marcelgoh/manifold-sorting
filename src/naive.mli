(* Naive implementation of Euclidean space *)

module Naive (Space : Space.Space) : Grid.Grid with type point := Space.point
