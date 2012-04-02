let rec h (ls1, ls2) = match (ls1, ls2) with
|h1::t1, h2::t2 -> (h1, h2) :: h (t1, t2)
|[],[] -> []
|_-> failwith "invalid lists";;