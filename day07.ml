let rec max(xs : int list) : int =
  if xs = [] then
    0
  else if List.hd xs > max(List.tl xs) then
    List.hd xs
  else
    max(List.tl xs)

let rec max2(xs : int list) : int =
  if xs = [] then
    0
  else
    let tl_answer = max2(List.tl xs) in
    if List.hd xs > tl_answer then
      List.hd xs
    else
      tl_answer

let rec max3(xs : int list) : int option = 
  if xs = [] then
    None
  else
    let tl_answer = max3(List.tl xs) in
    if tl_answer = None then
      Some(List.hd xs)
    else if List.hd xs > Option.get tl_answer then
      Some(List.hd xs)
    else
      tl_answer

let rec count_over_42(xs : int list) =
  if xs = [] then
    0
  else if List.hd xs > 42 then
    1 + count_over_42(List.tl xs)
  else
    0 + count_over_42(List.tl xs)

