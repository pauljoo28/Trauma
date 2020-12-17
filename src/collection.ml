type 'a collection = ('a * int) list

let empty = []

let get (i:'a) (c:'a collection) : int = 
    if List.mem_assoc i c then List.assoc i c else 0

let insert (k, v:'a*int) (c:'a collection) : 'a collection = 
    if List.mem_assoc k c then 
        let temp = List.assoc k c + v in
        (k, temp) :: List.remove_assoc k c
    else (k, v) :: List.remove_assoc k c

let rec add (c1:'a collection) (c2:'a collection) : 'a collection = 
    match c2 with
    | [] -> c1
    | h :: t -> add (insert (h) c1) t