let rec search
  graph (* the representation of the problem *)
  expand (* how to expand a node *)
  fringe (* the current nodes to be investigated *)
  goal (* specification of the solution *)
  strategy (* how to prioritise the fringe *)
  =
  
	List.iter (fun x->print_char x) fringe;
	print_endline ("");
  
  match fringe with
    | [] -> failwith "No solution"
    | hd :: tl ->
      if goal hd then hd
      else search graph expand (strategy tl (expand hd graph)) goal strategy
        
      

let roadmap =
[('A', 'Z', 75);
('A', 'S', 140);
('A', 'T', 118);
('T', 'L', 111);
('L', 'M', 70);
('M', 'D', 75);
('D', 'C', 120);
('C', 'R', 146);
('R', 'S', 80);
('R', 'P', 97);
('S', 'O', 151);
('O', 'Z', 71);
('S', 'F', 99);
('F', 'B', 211);
('B', 'P', 101);
('P', 'C', 138);
('B', 'G', 90);
('B', 'U', 85);
('U', 'H', 98);
('H', 'E', 86);
('U', 'V', 142);
('V', 'I', 92);
('I', 'N', 87)]


let rec expand vertex graph =
  match graph with
    | [] -> []
    | (v1, v2, _) :: tl ->
      if v1 = vertex then (v2 :: expand vertex tl)
      else if v2 = vertex then (v1 :: expand vertex tl)
      else expand vertex tl
  
let bfs_strategy oldf newf = oldf @ newf;;
let strategy oldf newf = newf @ oldf;;

(*search roadmap expand ['B'] (fun x->x="Z") strategy ;;*)
(*search roadmap expand ['A'] (fun x -> x = "D") (bfs_strategy);;*)
(*search roadmap expand ['A'] (fun x -> x = 'D' || x = 'F') (bfs_strategy [] = 'F');;*)