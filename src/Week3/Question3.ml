let empty ls = match ls with
|[]-> true
|_-> false;;

let rec lastelem ls = match ls with 
|[] -> failwith "empty"
|hd::tl -> if empty tl then hd else lastelem tl;;

let rec firstlasteq ls = match ls with
|[]-> false
|hd::tl -> if empty tl then true else if (hd = lastelem tl) then true else false;;