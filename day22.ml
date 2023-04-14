let f =
  fun x ->
    fun y -> x + y

let f x =
  fun y -> x + y

let f x y = x + y

let rec fold f acc xs =
  match xs with
  | [] -> acc
  | x :: xs' -> fold f (f(acc, x)) xs'

let my_sum = fold (fun (acc, next) -> acc + next) 0 [1;2;3;4;5]

let sum xs =
  fold (fun (acc, next) -> acc+next) 0 xs

let sum =
  fold (fun (acc, next) -> acc+next) 0

let rec fold f acc xs =
  match xs with
  | [] -> acc
  | x :: xs' -> fold f (f acc x ) xs'

  let sum =
    fold (fun acc next -> acc+next) 0
  