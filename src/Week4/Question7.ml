let rec aggregl l f d =
   match l with
   | [] -> d
   | x :: xs -> f x (aggregl xs f d)
