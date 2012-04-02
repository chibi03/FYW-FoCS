let rec sumlst ls = match ls with
|[] -> 0
|hd::tl -> hd + sumlst tl;;