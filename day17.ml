let rec map(f, xs) =
  match xs with
  | [] -> []
  | x :: xs' -> f x :: map(f, xs')

let data = [1;2;3;4;5]
let double x = x + x
let result = map(double, data)

let square_all(xs) =
  let square x = x*x
  in
  map(square, xs)

let square_result = square_all data

let square_all xs =
  map((fun x -> x*x), xs)

let square_result = square_all data

let square = fun x -> x*x

let square2 = square
let squared = square2 21

let rec filter(f, xs) =
  match xs with
  | [] -> []
  | x :: xs' -> 
    if f x then
      x :: filter(f, xs')
    else
      filter(f, xs')

let filter_result = filter((fun x -> x mod 2 = 0), [1;2;3;4;5;6])

let double_even_values(xs) =
    map((fun x -> x+x), filter((fun x -> x mod 2 = 0), xs))

let double_even_result = double_even_values([1;2;3;4;5;6])

let partition(xs, pivot) =
    (
    filter((fun x -> x < pivot), xs),
    filter((fun x -> x > pivot), xs)
    )
let partitioned = partition([12; 94; 82; 18; 83; 14; 34; 42], 50)

let double_or_triple f =
  if f 7 then
    fun x -> x * 2
  else
    fun x -> x * 3

let triple = double_or_triple (fun x -> x mod 2 = 0)
let four_tripled = triple 4

(* Lexical scope *)
let x = 1
let f y = x + y
let x = 2
let res1 = f(2) (* f(2) evals to 3 (x is 1 and y is 2) *)
let y = 3
let z = f(x+y)