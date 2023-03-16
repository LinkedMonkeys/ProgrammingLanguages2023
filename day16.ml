let function_applier(f, x) =
  f(x)
let double n = n + n
let doubled = function_applier(double, 21)

let square n = n * n
let squared = function_applier(square, 21)
let rec stars n =
  if n=0 then ""
  else "*" ^ stars(n-1)

let n_stars = function_applier(stars, 21)

let function_applier2(f, g, xs) =
  if List.length xs mod 2 = 0 then
    f xs
  else
    g xs

let rec list_sum xs =
  match xs with
  | [] -> 0
  | x :: xs' -> x + list_sum xs'

let rec list_product xs =
  match xs with
  | [] -> 1
  | x :: xs' -> x * list_product xs'
let result = function_applier2(list_sum, list_product, [1;2;3;4;5])

(* let result2 = function_applier2(List.hd, List.tl, [1;2;3;4;5]) *)

let rec map(f, xs) =
  match xs with
  | [] -> []
  | x :: xs' -> f x :: map(f, xs')

let doubled_list = map(double, [1;2;3;4;5])
let stars_list = map(stars, [1;2;3;4;5])

let rec filter(f, xs) =
  match xs with
  | [] -> []
  | x :: xs' ->
    if f x then x :: filter(f, xs')
    else filter(f, xs')
let even n = n mod 2 = 0
let evens = filter(even, [1;2;3;4;5])

let odds = let odd n = n mod 2 = 1
  in filter(odd, [1;2;3;4;5])

let odds2 = filter((fun n -> n mod 2 = 1), [1;2;3;4;5])

let odd = fun n -> n mod 2 = 1

let is_42_odd = odd 42

let double_evens(xs) =
  map((fun x -> x+x), filter((fun x -> x mod 2 = 0), xs))

let result = double_evens([1;2;3;4;5]) (* [4; 8] *)

let keep_strings_in_range(xs, lo, hi) =
  filter((fun s -> String.length s >= lo && String.length s <= hi), xs)

let result = keep_strings_in_range(["one";"two";"three"; "four"; "five"; "six"; "seven"], 4, 6)
(* ["three", "four", "five", "seven"] *)