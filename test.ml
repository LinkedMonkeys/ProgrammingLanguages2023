let rec pow((base: int), (exponent: int)) =
  if exponent = 0
    then 1
    else base * pow(base, exponent-1)

let result = pow(2, 10)