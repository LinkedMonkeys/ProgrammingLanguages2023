let rec zip xs =
  match xs with
  | ([], []) -> []
  | ([], _) -> []
  | (_, []) -> []
  | (x :: xs', y :: ys') -> (x, y) :: zip(xs', ys')

(* result = [(1, 4); (2, 5); (3, 6)] *)
let result = zip([1; 2; 3], [4; 5; 6])

let rec zip3 xs =
  match xs with
  | ([], [], []) -> []
  | ([], _, _) -> []
  | (_, [], _) -> []
  | (_, _, []) -> []
  | (x :: xs', y :: ys', z :: zs') -> (x, y, z) :: zip3(xs', ys', zs')