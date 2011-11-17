let rec split ls = match ls with
  | [] -> [], [], []
  | hd :: [] -> [hd], [], []
  | hd1 :: hd2 :: [] -> [hd1], [hd2], []
  | hd1 :: hd2 :: hd3 :: tl ->
    let ls1, ls2, ls3 = split tl in
    (hd1::ls1, hd2::ls2, hd3::ls3)