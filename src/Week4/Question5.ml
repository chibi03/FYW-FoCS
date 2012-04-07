let prodl =
   let rec prodl_inner accum = function
      | [] -> accum
      | x :: xs -> prodl_inner (x * accum) xs
   in
      prodl_inner 1
