let rec zip pr =
match pr with
| ([], []) -> []
| ([], _ ) -> []
| (_, []) -> []
| (x :: xs', y :: ys') -> (x, y) :: zip(xs', ys')

let rec unzip xs =
  match xs with
  | [] -> ([], [])
  | (x, y) :: rest ->
    match unzip rest with
    |(a, b) -> (x :: a, y::b)

let unzipped = unzip [(1, 5); (2, 6); (3, 7); (4, 8)]
let zipped = zip(unzip [(1, 5); (2, 6); (3, 7); (4, 8)])

let rec sum xs =
  match xs with
  | [] -> 0
  | x :: xs' -> x + sum xs'

let rec sum_kernel(xs, acc) =
  match xs with
  | [] -> acc
  | x :: xs' -> sum_kernel(xs', x + acc)

let result = sum_kernel([1; 2; 3], 0)

let sum2 xs =
  let rec sum_kernel(xs, acc) =
    match xs with
    | [] -> acc
    | x :: xs' -> sum_kernel(xs', x + acc)
  in
  sum_kernel(xs, 0)

  let result2 = sum2 [1; 2; 3]