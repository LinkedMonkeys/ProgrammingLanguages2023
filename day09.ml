type shape = 
| Circle of float * float * float (* x, y, radius *)
| Rectangle of float * float * float * float (* x1, y1 and x2, y2 *)
| Triangle of float * float * float * float * float * float

let my_circle = Circle (10.0, 10.0, 50.0)

let straight_sides(s : shape) : int =
    match s with
    | Circle(x, y, r) -> 0
    | Rectangle(x1, y1, x2, y2) -> 4
    | Triangle(x1, y1, x2, y2, x3, y3) -> 3

let circle_sides = straight_sides my_circle


let perimeter(s) =
  let length(x1, y1, x2, y2) =
    let x_diff = x2 -. x1 in
      let y_diff = y2 -. y1 in
        Float.sqrt(x_diff*.x_diff +. y_diff*.y_diff)
  in
  match s with
  | Circle(x, y, r) -> 2.0 *. Float.pi *. r
  | Rectangle(x1, y1, x2, y2) -> 2.0 *. length(x1, y1, x2, y1) +. 2.0 *. length(x1, y1, x1, y2)
  | Triangle(x1, y1, x2, y2, x3, y3) -> length(x1, y1, x2, y2) +. length(x2, y2, x3, y3) +. length(x3, y3, x1, y1)

  let c1 = Circle(10.0, 20.0, 2.0)
  let r1 = Rectangle(1.0, 1.0, 2.0, 2.0)
  let t1 = Triangle(0.0, 0.0, 4.0, 0.0, 0.0, 3.0)

  let c1_perimeter = perimeter c1
  let r1_perimeter = perimeter r1
  let t1_perimeter = perimeter t1

type expr =
| Constant of int
| Negate of expr
| Add of expr * expr
| Multiply of expr * expr

let e1 = Constant 3
let e2 = Multiply(Constant(5), Constant(7))
let e3 = Add(Constant(3), Multiply(Constant(5), Constant(7)))
