let rec factorial n =
    match n with
    | 0 -> 1
    | _ -> factorial(n-1) * n

let result = factorial 5 (* 120 *)

let rec factorial_kernel(n, acc) =
  match n with
  | 0 -> acc
  | _ -> factorial_kernel(n-1, n * acc)

let result2 = factorial_kernel(5, 1)

let factorial2 n =
  let rec factorial_kernel(n, acc) =
    match n with
    | 0 -> acc
    | _ -> factorial_kernel(n-1, n * acc)
  in
    factorial_kernel(n, 1)

let result3 = factorial2 5

let sum xs =
  let rec sum_kernel(xs, acc) =
    match xs with
    | [] -> acc
    | x :: xs' -> sum_kernel(xs', x + acc)
  in
  sum_kernel(xs, 0)

let list_sum = sum [1;2;3;4;5]

let rec reverse xs =
  match xs with
  | [] -> []
  | x :: xs' -> (reverse xs') @ [x]

let reversed = reverse [1;2;3;4;5]

let reverse2 xs =
  let rec reverse_kernel(xs, acc) =
  match xs with
  | [] -> acc
  | x :: xs' -> reverse_kernel(xs', x :: acc)
  in
  reverse_kernel(xs, [])

let reversed2 = reverse2 [1;2;3;4;5]