let rec projl = function
   | [] -> []
   | (a, b) :: xs -> a :: (projl xs)
