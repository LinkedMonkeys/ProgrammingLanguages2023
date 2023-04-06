let rec fold(f, acc, xs) =
  match xs with
  | [] -> acc
  | x :: xs' -> fold(f, f(acc, x), xs')

let list_length(xs) =
  fold((fun (acc, next) -> acc+1), 0, xs)

let my_list = [1;2;3;4;5]
let my_list_length = list_length my_list


let list_max xs =
  match xs with
  | [] -> failwith "Empty list"
  | x :: xs' -> fold((fun (acc, next) -> if next > acc then next else acc), x, xs)

let my_list_max = list_max my_list
let my_good_list = [18; 17; 28; 42; 8; 12]
let my_good_list_max = list_max my_good_list
(* let failure = list_max [] *)

let map(f, xs) =
  List.rev(fold((fun (acc, next) -> f next :: acc), [], xs))

let double_list xs =
  map((fun x -> x+x), xs)
let my_doubled_list = double_list my_list

let filter(f, xs) =
  List.rev(fold((fun (acc, next) -> if f next then next :: acc else acc), [], xs))

let keep_evens xs =
  filter((fun x -> x mod 2 = 0), xs)

let my_evens = keep_evens my_list

type int_tree =
| Null
| Node of int_tree * int * int_tree

let my_tree = Node(Node(Node(Null, 4, Null), 2, Null), 1, Node(Null, 3, Node(Null, 5, Null)))
let rec tree_map(f, t) =
  match t with
  | Null -> Null
  | Node(left, i, right) -> Node(tree_map(f, left), f i, tree_map(f, right))

let my_doubled_tree = tree_map((fun x -> x+x), my_tree)

let rec tree_fold(f, acc, t) =
  match t with
  | Null -> acc
  | Node(left, i, right) ->
    let new_acc = tree_fold(f, f(acc, i), left)
    in
    tree_fold(f, new_acc, right)

let tree_size t =
  tree_fold((fun (acc, next) -> acc+1), 0, t)

let my_tree_size = tree_size my_tree

let my_tree_sum = tree_fold((fun (acc, next) -> acc+next), 0, my_tree)