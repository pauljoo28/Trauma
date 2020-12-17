open Collection

type 'a trace = Base of 'a collection | Ind of ('a trace list * int)

let empty : 'a trace = Base(Collection.empty)

let add_diff (col:'a collection) (trace2:'a trace) : 'a trace =
    match trace2 with
    | Ind (t, d) -> 
        if d = 1 then Ind ((Base col:: t), d) else
        failwith "Must add diff to onl  y dimension 1 traces"
    | _ -> failwith "Must add dimension before adding diff"

let add_dim (tr:'a trace) : 'a trace =
    match tr with
    | Base c -> Ind ([Base c], 1)
    | Ind (t, d) -> Ind ([tr], d+1)

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
    | h :: t -> concat_all t (h @ acc)

and reverse (lst: (int list) list) : (int list) list =
    List.map (fun x -> List.rev x) lst

let rec get_version_helper (k:(int list) list) (tr:'a trace) (acc:'a collection) : 'a collection =
    match k with
    | [] -> acc
    | h :: t -> 
        let diff = get_diff_version h tr in
        match diff with
        | Base c -> get_version_helper t tr (Collection.add c acc)
        | Ind _ -> failwith "Diff was not a collection. Try checking the time and dimension of trace."

let get_version (k:int list) (tr:'a trace) : 'a trace =
    Base (get_version_helper (versions k) tr Collection.empty)