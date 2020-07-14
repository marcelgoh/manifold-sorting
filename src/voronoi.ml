

module Voronoi (Space : Space.Space) (Grid : Grid.Grid with type point := Space.point) = struct
  type line_segment = (float * float) * (float * float)
  type point = Space.point

  let get_vertices grid : (point * point * point) list = []

  let build (grid : Grid.grid) : line_segment list = []
end
