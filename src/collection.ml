type collection = (int * int) list

let empty = []
let get i c = 
    if List.mem_assoc i c then List.assoc i c else 0
let insert c1 c2 = 
    let indices = List.append (fst (List.split c1)) (fst (List.split c2)) in
    let unique = List.sort_uniq (fun a b -> if a < b then -1 else if a = b then 0 else -1) indices in
    List.map (fun i -> (i, (get i c1) + (get i c2))) unique
let make pairs = pairs
let to_list col = col