(* Start off using this one for a "guess the signature". *)
(* Same as last time? *)
let rec func1(f, pr) = 
  match pr with
  | ([], []) -> []
  | (x::xs, []) -> []
  | ([], y::ys) -> []
  | (x::xs, y::ys) -> f(x, y) :: func1(f, (xs, ys))

(* Another "determine the types"*)
let rec func2 xs =
  let rec inner_func2 x =
    if x = 0 then
      []
    else
      x :: inner_func2(x-1)
  in
  match xs with
  | [] -> []
  | x :: xs' -> inner_func2 x :: func2 xs'

let rec func3(xs) =
  match xs with
  | [] -> []
  | (x, y) :: xs' -> x+y :: func3 xs'

let rec func4(f1, f2, xs) =
  match xs with
  | [] -> []
  | x :: xs' -> (f1 x, f2 x) :: func4(f1, f2, xs')

let rec func5(f1, f2, xs) =
  match xs with
  | [] -> []
  | x1 :: x2 :: xs' -> f1 x1 :: f2 x2 :: func5(f1, f2, xs')
  | x :: [] -> [f1 x]

(* let rec func6(f1, f2, xs) =
  match xs with
  | x :: xs' ->  f2((f1 xs), func6(f1, f2, xs')) *)

(* Function for "determine the types". *)
let f n =
  let rec inner xs =
    match xs with
    | [] -> []
    | x :: xs' -> (n*x) :: inner xs'
  in inner

let rec f2(n, xs) =
    match xs with
    | [] -> []
    | x :: xs' -> (n*x) :: f2(n, xs')

let my_list = [1;2;3;4;5]
let my_doubled_list = f2(2, my_list)
let list_doubler = f 2
let my_doubled_list2 = list_doubler my_list