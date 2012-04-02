let rec selectA ls = match ls with
|[]->[]
|(a,b):: tl -> a :: selectA tl;;

let rec selectB ls = match ls with
|[]->[]
|(a,b):: tl -> b :: selectB tl;;

let h ls = selectA ls, selectB ls;;