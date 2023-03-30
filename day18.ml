let x = 1
let y = x + 10
let x = 10
let z = y
(* z is 11 or is it 20? *)

let x = 1
let f() = x
let y = f()
let x = 42
let z = f()


let x = 1
let y = 2
let f() = x + y

let x = 1
let f y = x + y
let x = 2
let y = 3
let z = f(x+y)
let z = f(5)

let rec fold(f, acc, xs) =
  match xs with
  | [] -> acc
  | x :: xs' -> fold(f, f(acc, x), xs')

let result = fold((fun(acc, next) -> acc + next), 0, [1;2;3;4;5])

let list_sum(xs) =
  fold((fun (acc, next) -> acc + next), 0, xs)

  (* Are all of the lights on? *)
let lights_on(xs) =
  fold((fun (acc, next) -> acc && next), true, xs)
let all_lights_on = lights_on([true; true; true; true; true])

let first_char_evens(xs) =
  fold((fun (acc, next) -> if (String.length next) mod 2 = 0 then acc ^ (String.sub next 0 1) else acc)
    , "", xs)

let first_chars = first_char_evens(["one"; "two"; "three"; "four"; "five"; "six"])
(* "ff" *)

let numbers_in_range(xs, lo, hi) =
  fold(
    (fun (acc, next) -> if next <= hi && next >= lo then acc+1 else acc)
    , 0, xs)

let result = numbers_in_range([39; 199; 82; 12; 19; 42; 99], 30, 100)

let all_are_shorter(xs, s) =
  fold(
    (fun (acc, next) -> acc && (String.length next) < (String.length s)),
    true,
    xs
  )
let result = all_are_shorter(["one"; "two"; "three"; "four"; "five"; "six"], "test12")