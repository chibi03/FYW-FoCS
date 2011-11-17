 let rec revert ls = match ls with
| [] -> []
| hd :: tl -> (revert tl) @ [hd];;

let rec mulWithOverflow ls1 ls2 overflow = 
	let multiplication hd1 tl1 hd2 tl2 overflow =
      let sum = (hd1 * hd2) + overflow in
      if sum >= 10 then 
		let remainder = sum - int_of_float((float_of_int sum)/.10.)*10 in
        let overflow = (int_of_float((float_of_int sum)/.10.)*10)/10 in
        (mulWithOverflow tl1 tl2 overflow) @ [remainder]
      else
        (mulWithOverflow tl1 tl2 0) @ [sum] in
  
	match ls1, ls2 with
      | [], [] -> if overflow != 0 then [overflow] else []
      | [], hd::tl | hd::tl, [] -> multiplication hd tl 0 [] overflow
                   | hd1::tl1, hd2::tl2 -> multiplication hd1 tl1 hd2 tl2 overflow;;

let rec mul ls1 ls2 = mulWithOverflow (revert ls1) (revert ls2) 0;;