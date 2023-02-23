type coordinate = {
  x : float;
  y : float;
}

type shape =
| Circle of coordinate * float
| Rectangle of coordinate * coordinate
| Triangle of coordinate * coordinate * coordinate

let c = Circle({x=1.0; y=2.0}, 10.0)
let r = Rectangle({x=10.0; y=10.0}, {x=20.0; y=2.0})
let t = Triangle({x=0.0; y=0.0}, {x=3.0; y=0.0}, {x=0.0; y=4.0})

let perimeter(s) =
  let length(c1, c2) =
    let x_diff = c2.x -. c1.x in
      let y_diff = c2.y -. c1.y in
        Float.sqrt(x_diff*.x_diff +. y_diff*.y_diff)
  in
  match s with
  |Circle(c, r) -> 2.0 *. Float.pi *. r
  |Rectangle(c1, c2) -> 2.0 *. (length(c1, {x=c2.x; y=c1.y}) +. length(c1, {x=c1.x; y=c2.y}))
  |Triangle(c1, c2, c3) -> length(c1, c2) +. length(c2, c3) +. length(c3, c1)

type expr =
| Constant of int
| Negate of expr
| Add of expr * expr
| Multiply of expr * expr

let thirty_eight_expr = Add(Constant 3, Multiply(Constant 5, Constant 7))

let rec eval(e) =
match e with
| Constant i -> i
| Negate e -> -1 * eval e
| Add(e1, e2) -> eval e1 + eval e2
| Multiply(e1, e2) -> eval e1 * eval e2

let thirty_eight = eval thirty_eight_expr
let negative_thirty_eight_expr = Negate thirty_eight_expr
let negative_thirty_eight = eval negative_thirty_eight_expr

let rec max_constant(e) =
let max(a, b) = if b > a then b else a
in
match e with
| Constant i -> i
| Negate e -> max_constant e
| Add(e1, e2) -> max(max_constant e1, max_constant e2)
| Multiply(e1, e2) -> max(max_constant e1, max_constant e2)

let max_constant_thirty_eight_expr = max_constant(thirty_eight_expr)

let max_constant_negative_thirty_eight_expr = max_constant(negative_thirty_eight_expr)

let rec count_adds(e) =
match e with
| Constant i -> 0
| Negate e -> count_adds e
| Add(e1, e2) -> 1 + count_adds e1 + count_adds e2
| Multiply(e1, e2) -> count_adds e1 + count_adds e2

let my_list = [1;2;3;4;5]

let rec sum_list(xs : int list) : int =
if xs = [] then
  0
else
  List.hd xs + sum_list(List.tl xs)

let rec sum_list2(xs) =
  match xs with
  | [] -> 0
  | x :: xs' -> x + sum_list2 xs'

  