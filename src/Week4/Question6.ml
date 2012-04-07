let rec flatten = function
   | [] -> []
   | x :: xs -> x @ (flatten xs)
