let first ls = match ls with
|[]->failwith "no second element"
|hd::tl-> hd;;

let firsteq ls = match ls with
|[]-> false
|hd::tl -> if (hd = first tl) then true else false;;