type natural =
| Zero
| Successor of natural

let zero = Zero
let one = Successor(Zero)
let two = Successor(one)
let two2 = Successor(Successor(Zero))

let rec int_of_natural(n) =
match n with
| Zero -> 0
| Successor(n') -> 1 + int_of_natural n'
 
let rec int_list_length(xs) =
  match xs with
  | [] -> 0
  | _ :: xs' -> 1 + int_list_length xs'

let two_int = int_of_natural(two) (* 2 *)

let rec add(a, b) =
match a with
| Zero -> b
| Successor(a') -> Successor(add(a', b))

let rec append(xs, ys) =
  match xs with
  | [] -> ys
  | x :: xs' -> x :: append(xs', ys)

let four = add(two, two2)

let four_int = int_of_natural(add(two, two))

type my_list =
| Empty
| Cons of int * my_list

let l = Cons(1, Cons(2, Cons(3, Empty)))

let l2 = 1 :: (2 :: (3 :: []))

let l3 = [1; 2; 3]

let rec len(xs) =
match xs with
| Empty -> 0
| Cons(_, xs') -> 1 + len(xs')

type tree_node =
| Empty
(* | Leaf of int *)
| Parent of tree_node * int * tree_node

let our_tree = Parent(Parent(Parent(Empty, 4, Empty),2, Parent(Empty, 5, Empty)),
 1,
 Parent(Empty, 3, Parent(Empty, 6, Empty)))

let rec tree_sum(tree) =
  match tree with
  | Empty -> 0
  | Parent(left, data, right) -> tree_sum(left) + data + tree_sum(right)

  let twenty_one = tree_sum our_tree

type expr =
| Constant of int
| Negate of expr
| Add of expr * expr
| Multiply of expr * expr

let rec eval(e) =
match e with
| Constant(i) -> i
| Negate(e1) -> -1 * eval(e1)
| Add(e1, e2) -> eval(e1) + eval(e2)
| Multiply(e1, e2) -> eval(e1) * eval(e2)

let sixteen = eval(Add(Constant 1, Multiply(Constant 3, Constant 5)))

let rec count_adds(e) =
match e with
| Constant(_) -> 0
| Negate(e1) -> count_adds e1
| Add(e1, e2) -> 1 + count_adds e1 + count_adds e2
| Multiply(e1, e2) -> count_adds e1 + count_adds e2

let rec has_const_not_under_addition(e) : bool =
match e with
| Constant(_) -> true
| Negate(e1) -> has_const_not_under_addition e1
| Add(e1, e2) -> false
| Multiply(e1, e2) -> has_const_not_under_addition e1 || has_const_not_under_addition e2