let compose f g x = f (g x)

let compose =
  fun f ->
    fun g ->
      fun x -> f (g x)

let compose f =
  fun g ->
    fun x -> f (g x)

let double_and_add_two = compose (fun x -> x + 2) (fun y -> y + y)

let forty_two = double_and_add_two 20

let (%) = compose
let double_and_add_two = (fun x -> x + 2) % (fun y -> y + y)
let forty_two = double_and_add_two 20
