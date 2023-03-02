let sum_triple(triple : int * int * int) =
match triple with
| (a, b, c) -> a + b + c

let my_triple = (1, 2, 3)
let six = sum_triple(my_triple)

(* let rec list_sum(xs) =
match xs with
| [] -> 0
| x :: xs' -> x + list_sum xs' *)

let sum_triple2(triple: int * int * int) =
  let (a, b, c) = triple
  in 
  a + b + c

let sixv2 = sum_triple2(my_triple)

let sum_triple3(a, b, c) = 
  a + b + c

let sixv3 = sum_triple3(my_triple)

let fst3(a, _, _) = a

let first = fst3(1, 2, 3)

let rotate_triple_left(a, b, c) = (b, c, a)

let rotate_triple_right(triple) =
  rotate_triple_left(rotate_triple_left triple)

let my_triple_left = rotate_triple_left my_triple
let my_triple_right = rotate_triple_right my_triple

let forty_two() = 42

let rec zip(pair_of_lists) =
match pair_of_lists with
| ([], []) -> []
| ([], _) -> []
| (_, []) -> []
| (x :: xs, y :: ys) -> (x, y) :: zip(xs, ys)

let my_data = ([1; 2; 3], [7; 8; 9])
let my_data_zipped = zip my_data