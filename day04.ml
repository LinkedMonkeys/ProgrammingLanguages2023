let pair_sum(pair : int * int) =
  fst pair + snd pair

let sort_pair(pair : int * int) =
  if fst pair < snd pair then
    pair
  else
    (snd pair, fst pair)

let div_mod((dividend : int), (divisor: int)) =
  (dividend / divisor, dividend mod divisor)

let second_of_list(xs : int list) =
  List.hd(List.tl xs)

let rec count_list(xs : int list) =
  if xs = [] then
    0
  else
    count_list(List.tl xs) + 1

let result = count_list([1;2;3;4;5])

let rec count_down(n : int) =
  if n = 0 then
    []
  else
    n :: count_down(n-1)

let result = count_down(10)

let rec list_sum(xs : int list) =
  if xs = [] then
    0
  else
    List.hd xs + list_sum(List.tl xs)

let rec append((xs : int list), (ys : int list)) =
  if xs = [] then
    ys
  else
    List.hd xs :: append(List.tl xs, ys)

    let result = append([1;2;3;4], [5;6;7;8])
(* [1;2;3;4;5;6;7;8]*)

let rec double(xs : int list) =
  if xs = [] then
    []
  else
    List.hd xs * 2 :: double(List.tl xs)





    