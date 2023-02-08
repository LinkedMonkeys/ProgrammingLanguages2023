



let rec is_in((xs : int list), (key : int)) : bool =
  if xs = [] then
    false
  else if key = List.hd xs then
    true
  else
    is_in(List.tl xs, key)


let x = [1; 2; 3; 4; 5]
let z = is_in(x, 42) (* false *)
let z2 = is_in(x, 2) (* true *)

(* Assume n is "in-bounds". *)
let rec get_nth((xs : int list), (n : int)) : int =
  if n = 0 then
    List.hd xs
  else
    get_nth(List.tl xs, n-1)

let z3 = get_nth(x, 3) (* 4 *)

let rec get_nth2((xs : int list), (n : int)) : bool * int =
  if xs = [] then
    (false, -999999)
  else if n = 0 then
    (true, List.hd xs)
  else
    get_nth2(List.tl xs, n-1)

let rec pair_list_sum(xs : (int * int) list) : int * int =
  if xs = [] then
    (0, 0)
  else
    (fst(List.hd xs) + fst(pair_list_sum(List.tl xs)),
    snd(List.hd xs) + snd(pair_list_sum(List.tl xs)))
