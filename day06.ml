let pair_sum((a : int * int), (b : int * int)) : int * int =
  (fst a + fst b, snd a + snd b)

let rec pair_list_sum(xs : (int * int) list) : int * int =
  if xs = [] then
    (0, 0)
  else
    pair_sum(List.hd xs, pair_list_sum(List.tl xs))

      (***********************************)
let rec pair_list_sum4(xs : (int * int) list) : int * int = 
  if xs = [] then
    (0, 0)
  else
    let pair_sum2((a : int * int), (b : int * int)) : int * int =
      (fst a + fst b, snd a + snd b)
    in
    pair_sum2(List.hd xs, pair_list_sum4(List.tl xs))
      (***********************************)


let rec firsts(xs: (int * int) list) : int list =
    if xs = [] then
      []
    else
      fst(List.hd xs) :: firsts(List.tl xs)

let rec seconds(xs: (int * int) list) : int list =
  if xs = [] then
    []
  else
    snd(List.hd xs) :: seconds(List.tl xs)
    
let rec list_sum(xs : int list) : int =
  if xs = [] then
    0
  else
    List.hd xs + list_sum(List.tl xs)

let pair_list_sum2(xs : (int * int) list) : int * int =
  (list_sum(firsts xs), list_sum(seconds xs))

let rec pair_list_sum3(xs : (int * int) list) : int * int =
    if xs = [] then
      (0, 0)
    else
      let tl_answer = pair_list_sum3(List.tl xs) in
      (fst(List.hd xs) + fst(tl_answer), snd(List.hd xs) + snd(tl_answer))

let count_up(n : int) : int list =
  let rec count_up_from((from : int), (tox : int)) : int list = 
    if from >= tox+1 then
      []
    else
      from :: count_up_from(from+1, tox)
  in
  count_up_from(1, n)

  let count_up(n : int) : int list =
    let rec count_up_from(from : int) : int list = 
      if from >= n+1 then
        []
      else
        from :: count_up_from(from+1)
    in
    count_up_from(1)
    
  