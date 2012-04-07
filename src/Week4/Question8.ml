let rec transpose l =
   let rec applyl f = function
      | [] -> []
      | x :: xs -> (f x) :: (applyl f xs)
   and foldl f d = function
      | [] -> d
      | x :: xs -> f x (foldl f d xs)
   and exht = function
      | [] -> raise (Invalid_argument "transpose")
      | x :: xs -> (x, xs)
   and fht = function
      | [] -> ([], [])
      | (h, t) :: xs -> let (hs, ts) = fht xs in (h :: hs, t :: ts)
   in
      match l with
      | [] -> []
      | _ when (foldl ( && ) true (applyl (fun x -> x = []) l)) -> []
      | _ -> let (f, r) = fht (applyl exht l) in f :: (transpose r)
