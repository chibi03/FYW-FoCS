let perm a b =
   let rec remove e accum = function
      | [] -> accum
      | x :: xs when (x = e) -> accum @ xs (* don't care about order *)
      | x :: xs -> remove e (x :: accum) xs
   in
   let rec perm_inner = function
      | [], [] -> true
      | [], _ | _, [] -> false
      | x :: xs, l -> perm_inner (xs, (remove x [] l))
   in
      perm_inner (a, b)

(* Testing *)
let () = Printf.printf "%b\n" (perm [1;2;3;4] [3;2;1;3])
