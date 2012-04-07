let rec applyl l f =
   match l with
   | [] -> []
   | x :: xs -> (f x) :: (applyl xs f)
