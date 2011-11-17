let rec merge = function 
  | ls, [], [] | [], ls, [] | [], [], ls -> ls
  | hd1::tl1, hd2::tl2, [] | [], hd1::tl1, hd2::tl2 | hd1::tl1, [], hd2::tl2->
    if hd1 < hd2
    then hd1 :: merge (tl1, hd2::tl2, [])
    else hd2 :: merge (hd1::tl1, tl2, [])
  | hd1::tl1, hd2::tl2, hd3::tl3 ->
    if hd1 < hd2 && hd1 < hd3 then
      hd1 :: merge (tl1, hd2::tl2, hd3::tl3)
    else if hd2 < hd1 && hd2 < hd3 then
      hd2 :: merge (hd1::tl1, tl2, hd3::tl3)
    else
      hd3 :: merge (hd1::tl1, hd2::tl2, tl3)
      
let rec split ls = match ls with
  | [] -> [], [], []
  | hd :: [] -> [hd], [], []
  | hd1 :: hd2 :: [] -> [hd1], [hd2], []
  | hd1 :: hd2 :: hd3 :: tl ->
    let ls1, ls2, ls3 = split tl in
    (hd1::ls1, hd2::ls2, hd3::ls3)
      
let rec mergesort = function
  | [] -> []
  | [hd] -> [hd]
  | ls ->
    let (ls1, ls2, ls3) = split ls in
    let ls1 = mergesort ls1 in
    let ls2 = mergesort ls2 in
    let ls3 = mergesort ls3 in
    merge (ls1, ls2, ls3)
    
let perm ls1 ls2 =
  let ls1 = mergesort ls1 in
  let ls2 = mergesort ls2 in
  (compare ls1  ls2) == 0;;