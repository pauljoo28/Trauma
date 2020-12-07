open Collection

type trace = collection list

let empty = []
let make lst = lst

let rec sum_help k lst aux = match lst with
    | [] -> aux
    | h :: t -> if (k == 0) then aux else 
        sum_help (k-1) t (Collection.insert aux h)

let version k lst = sum_help k lst Collection.empty

let to_list t = t