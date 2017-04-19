

let func_sans_args () = 5+2

let func_avec_un_arg x = x = 2

let func_avec_deux_args x y = x * y

let average a b =  (a +. b) /. 2.0

let square x = x * x

let valeur_absolue x = if x >= 0 then x else - x

let max a b =
  if a > b then a else b
  
let rec fact x =
  if x <= 1 then 1 else x * fact (x - 1)

let rec range a b =
    if a > b then []
    else a :: range (a+1) b

let rec fib x = if x <= 1 then 1 else fib (x - 1) + fib (x - 2)    

