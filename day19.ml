let rec map(f, xs) =
  match xs with
  | [] -> []
  | x :: xs' -> f x :: map(f, xs')

let rec filter(f, xs) =
  match xs with
  | [] -> []
  | x :: xs' -> 
    if f x then
      x :: filter(f, xs')
    else
      filter(f, xs')
  
let rec fold(f, acc, xs) =
  match xs with
  | [] -> acc
  | x :: xs' -> fold(f, f(acc, x), xs')

let join(ss, separator) =
  fold((fun (acc, next) -> acc ^ separator ^ next), "", ss)

let join(ss, separator) =
  match ss with
  | [] -> ""
  | s :: ss' -> fold((fun (acc, next) -> acc ^ separator ^ next), s, ss')
let word_list = ["Now"; "is"; "the"; "time"]
let sentence = join(word_list, "*")

let int_list = [1;2;3;-4;5]
let negate_list xs =
  map((fun x -> -x), xs)

let negative_int_list = negate_list int_list
(* [-1;-2;-3;4;-5] *)

let negate_list2 xs =
  fold((fun (acc, next) -> -next :: acc), [], xs)

(* Need fold right! *)
  let negative_int_list2 = negate_list2 int_list

let add(a, b) =
  a + b

let add a =
  (* int -> int *)
  fun b -> a + b

let ten = add 7 3
(* let ten = (add 7) 3 *)

let increment = add 1
let forty_three = increment 42

let a = 99

let weird_result = increment 42

let reverse xs =
  fold((fun (acc, next) -> next :: acc),[], xs)
let prefix_sum xs =
  reverse(
    fold(
      (fun (acc, next) ->
        match acc with
        | [] -> [next]
        | x :: xs' -> x + next :: acc),
      [],
      xs
    )
  )

let num_list = [1; 3; 7; 4; 2; 8]

let result = prefix_sum num_list