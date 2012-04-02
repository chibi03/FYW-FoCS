let rec num ls = match ls with
|[]->0
|hd::tl -> 1 + num tl;;

let len3 ls = if (num ls >=3) then true else false;;