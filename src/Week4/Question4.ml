let suml =
   let rec suml_inner accum = function
      | [] -> accum
      | x :: xs -> suml_inner (x + accum) xs
   in
      suml_inner 0
