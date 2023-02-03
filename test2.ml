#utop_prompt_dummy
let _ = UTop.set_show_box false

let rec pow((base: int), (exponent: int)) =
  if exponent = 0
    then 1
    else base * pow(base, exponent-1)

let result = pow(2, 10)

let pair = (1, 2)
let pair2 = (1, false)

let num1 = fst pair2
let num2 = snd pair2

let triple = (1, 2, 3)

(* sum_of_pairs ((1, 2), (3, 4)) -> (4, 6)*)
let sum_of_pairs((a: int*int), (b: int*int)) =
    (fst a + fst b, snd a + snd b)

let sum = sum_of_pairs((1, 2), (3, 4))

let other_sum = sum_of_pairs((1, true), (3, false))








