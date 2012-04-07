let rec swapl = function
   | [] -> []
   | (a, b) :: xs -> (b, a) :: (swapl xs):
