let rec merge lss = match lss with
  | [], [], [] -> []
  | ls, [], [] 
  | [], ls, [] 
  | [], [], ls -> ls
  | hd1::tl1, hd2::tl2, [] 
  | [], hd1::tl1, hd2::tl2 
  | hd1::tl1, [], hd2::tl2 ->
	if hd1 < hd2
	then hd1 :: merge (tl1, hd2::tl2, [])
	else hd2 :: merge (hd1::tl1, tl2, [])
  | hd1::tl1, hd2::tl2, hd3::tl3 ->
    if hd1 < hd2 && hd1 < hd3 then
      hd1 :: merge (tl1, hd2::tl2, hd3::tl3)
    else if hd2 < hd1 && hd2 < hd3 then
      hd2 :: merge (hd1::tl1, tl2, hd3::tl3)
    else
      hd3 :: merge (hd1::tl1, hd2::tl2, tl3);;

let rec merge lss = match lss with
  | [], [], [] -> []
  | ls, [], [] 
  | [], ls, [] 
  | [], [], ls -> [],[],[]
  | hd1::tl1, hd2::tl2, [] 
  | [], hd1::tl1, hd2::tl2 
  | hd1::tl1, [], hd2::tl2 -> [],[],[]
  | hd1::tl1, hd2::tl2, hd3::tl3 -> [], [], []