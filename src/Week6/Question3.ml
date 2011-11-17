 let rec revert ls = match ls with
| [] -> []
| hd :: tl -> (revert tl) @ [hd];;

let rec addWithOverflow ls1 ls2 overflow = 
	let addition hd1 tl1 hd2 tl2 overflow =
      let sum = hd1 + hd2 + overflow in
      if sum >= 10 then 
		let remainder = sum - int_of_float((float_of_int sum)/.10.)*10 in
        let overflow = (int_of_float((float_of_int sum)/.10.)*10)/10 in
        (addWithOverflow tl1 tl2 overflow) @ [remainder]
      else
        (addWithOverflow tl1 tl2 0) @ [sum] in
  
	match ls1, ls2 with
      | [], [] -> if overflow != 0 then [overflow] else []
      | [], hd::tl | hd::tl, [] -> addition hd tl 0 [] overflow
                   | hd1::tl1, hd2::tl2 -> addition hd1 tl1 hd2 tl2 overflow;;

let rec add ls1 ls2 = addWithOverflow (revert ls1) (revert ls2) 0;;
add [1;2;3;4] [9;6];;