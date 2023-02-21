type item = {
  name : string;
  price : float;
  on_hand : int;
  num_sold : int;
}

let item1 : item = {
  name = "Water Bottle";
  price = 9.82 *. 1.25;
  on_hand = 10;
  num_sold = 0;
}

let item2 = {
  price = 1.14;
  name = "Whiteboard marker";
  num_sold = 27;
  on_hand = 48;
}

let the_price = item1.price

let rec count_inventory(xs : item list) : int =
  if xs = [] then
    0
  else
    (List.hd xs).on_hand + count_inventory(List.tl xs)

let total_items = count_inventory([item1; item2])

type si_unit =
| Meter
| Second
| Kilogram

let x : si_unit = Meter

let string_of_si_unit(z : si_unit) : string =
    match z with
    | Meter -> "meter"
    | Second -> "1/60th of a minute"
    | Kilogram -> "kilogram"

let result = string_of_si_unit x

type silly = 
| A of int * bool * (string list)
| Foo of string
| Pizza

(* type other_silly = 
| Pizza
| X of int

let silly_test1 = Pizza
let silly_test : silly = Pizza *)
let x : int = 42
let silly1 : silly = Pizza
let silly2 : silly = Foo "Hello"
let silly3 : silly = A (42, true, ["Now"; "is"; "the"; "time"])

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


