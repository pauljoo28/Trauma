open Collection

type 'a trace = Base of 'a collection | Ind of ('a trace list * int)


let debug_int_list_tostring (input:int list) : string =
    "(" ^ String.concat ", " (List.map (fun x -> string_of_int x) input) ^ ")"

let debug_int_list_list_tostring (input:int list list) : string =
    "[" ^ String.concat ", " (List.map (fun x -> debug_int_list_tostring x) input) ^ "]"

let init (col:'a collection) : 'a trace = Base(col)

let rec swap (time: int list) (col:'a collection) (tr: 'a trace) : 'a trace =
    match time with
    | [] -> (
        match tr with
        | Base c -> Base col
        | Ind _ -> failwith "Dimensions too small")
    | h :: t -> (
        match tr with
        | Base _ -> failwith "Dimension too large"
        | Ind (trl, d) -> Ind (List.mapi (fun i x -> if i=h then swap t col x else x) trl, d))

let to_collection (tr:'a trace) : 'a collection =
    match tr with
    | Base c -> c
    | Ind _ -> failwith "Should not have more than 1 dimension"

let rec add_diff_helper (col:'a collection) (trace2:'a trace) : 'a trace =
    match trace2 with
    | Ind (t, d) -> 
        if d = 1 then Ind ((t @ [Base col]), d) else
        failwith "Must add diff to only dimension 1 traces"
    | _ -> failwith "Must add dimension before adding diff"

and add_diff (k:int list) (col:'a collection) (trace2:'a trace) : 'a trace =
    match k with
    | [] -> add_diff_helper col trace2
    | h :: t -> 
        match trace2 with
        | Base _ -> failwith "Not enough dimensions"
        | Ind (tr', d) -> 
            Ind (List.mapi (fun i x -> if i = h then add_diff t col x else x) tr', d)

let add_dim (tr:'a trace) : 'a trace =
    match tr with
    | Base c -> Ind ([Base c], 1)
    | Ind (t, d) -> Ind (List.map (fun x -> Ind([x], d)) t, d+1)

let rec get_diff_version_helper1 (k:int) (tr:'a trace) : 'a trace =
    match tr with
    | Base c -> failwith "Trace has not enough dimensions"
    | Ind (tr', d) -> 
        if d = 1 then 
            match (List.nth tr' k) with 
            | Base c' -> Base c' 
            | Ind _ -> failwith "Not a dimension of 1"
        else 
            match (List.nth tr' k) with 
            | Base _ -> failwith "Dimension of 1"
            | Ind (tr'', d') -> Ind (tr'', d')

and get_diff_version_helper2 (k:int list) (tr:'a trace) : 'a trace =
    match k with
    | [] -> 
        (match tr with
        | Base c -> Base c
        | Ind x -> failwith "Too many dimensions remaining")
    | h :: t ->
        get_diff_version_helper2 t (get_diff_version_helper1 h tr)

let get_diff_version (k:int list) (tr:'a trace) : 'a trace =
    get_diff_version_helper2 k tr

(* This gets all the times before a time *)
let rec versions (time:int list) : ((int list) list) =
    reverse (versions_helper time [])

and versions_helper (time: int list) (acc : (int list) list) =
    match time with 
    | [] -> acc
    | h::t -> let nums = count_down h [] in 
        if List.length acc = 0 then versions_helper t (List.map (fun x -> [x]) nums) else
        let acc2 = concat_all (List.map (fun x -> versions_helper2 h x) acc) [] in
        versions_helper t acc2

and versions_helper2 (k:int) (acc2:int list) : (int list) list =
    let nums = count_down (k) [] in
    List.map (fun x -> x :: acc2) nums

and count_down (t : int) (acc : int list) : int list = 
    if (t = 0) then 0::acc else count_down (t-1) (t::acc)

and concat_all (all: ((int list) list) list) (acc:(int list) list) : ((int list) list) =
    match all with
    | [] -> acc
    | h :: t -> concat_all t (acc @ h)

and reverse (lst: (int list) list) : (int list) list =
    List.map (fun x -> List.rev x) lst

let rec get_version_helper (k:(int list) list) (tr:'a trace) (acc:'a collection) : 'a collection =
    match k with
    | [] -> acc
    | h :: t -> 
        let diff = try get_diff_version h tr with Failure _ -> Base(Collection.empty) in
        match diff with
        | Base c -> get_version_helper t tr (Collection.add c acc)
        | Ind _ -> failwith "Diff was not a collection. Try checking the time and dimension of trace."

let get_version (k:int list) (tr:'a trace) : 'a trace =
    Base (get_version_helper (versions k) tr Collection.empty)

let rec map (f: 'a collection -> 'a collection) (tr: 'a trace) : 'a trace =
    match tr with
    | Base c -> Base (f c)
    | Ind (tr, d) -> Ind (List.map (fun x -> map f x) tr, d)

let rec iter_helper1 (tr:'a trace list) (index:int) (acc:int list list) : int list list =
    match tr with
    | [] -> acc
    | h :: t -> iter_helper1 t (index+1) 
        (acc @ (List.map (fun x -> index :: x) (iter_helper2 h)))

and iter_helper2 (tr:'a trace) : int list list =
    match tr with
    | Base c -> [[]]
    | Ind (trl, d) -> iter_helper1 trl 0 []

let iter (tr: 'a trace) : int list list =
    iter_helper2 tr

let rec distinct (tr:'a trace) : 'a trace =
    distinct_helper_2 tr (empty_output tr) (iter tr)

and distinct_helper (itr:'a trace) (otr:'a trace) (time:int list) : 'a trace =
    let iversion = get_version time itr |> to_collection in
    let oversion = get_version time otr |> to_collection in
    swap time (Collection.subtract (Collection.distinct iversion) (oversion)) otr

and distinct_helper_2 (itr:'a trace) (otr:'a trace) (times:int list list) : 'a trace =
    match times with
    | [] -> otr
    | h :: t -> distinct_helper_2 itr (distinct_helper itr otr h) t

and empty_output (input:'a trace) : 'a trace =
    map (fun x -> Collection.empty) input

let get_dim (tr:'a trace) : int =
    match tr with
    | Base c -> 0
    | Ind (_, d) -> d

let debug_get_dim (tr:'a trace) : int =
    get_dim tr

let debug_iter_tostring (input:'a trace) : string =
    debug_int_list_list_tostring (iter input)

let debug_empty_output (input:'a trace) : 'a trace =
    empty_output input