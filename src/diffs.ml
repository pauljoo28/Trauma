type diffs = (int list, int) Hashtbl.t

let empty : diffs = Hashtbl.create 10000

let rec count_down (t : int) (acc : int list) : int list = 
    if (t = 0) then 0::acc else count_down (t-1) (t::acc)

let rec versions_helper2 (k:int) (acc2:int list) : (int list) list =
    let nums = count_down (k) [] in
    List.map (fun x -> x :: acc2) nums

and concat_all (all: ((int list) list) list) (acc:(int list) list) : ((int list) list) =
    match all with
    | [] -> acc
    | h :: t -> concat_all t (h @ acc)

and versions_helper (time: int list) (acc : (int list) list) =
    match time with 
    | [] -> acc
    | h::t -> let nums = count_down h [] in 
        if List.length acc = 0 then versions_helper t (List.map (fun x -> [x]) nums) else
        let acc2 = concat_all (List.map (fun x -> versions_helper2 h x) acc) [] in
        versions_helper t acc2

and reverse (lst: (int list) list) : (int list) list =
    List.map (fun x -> List.rev x) lst
        
and versions (time:int list) : ((int list) list) =
    reverse (versions_helper time [])

let accumulate (lst : (int list) list) (d:diffs) : (int) =
    List.fold_left (fun x y -> x + (Hashtbl.find d y)) 0 lst

let total (d:diffs) (time:int list) : int =
    accumulate (versions time) d

let insert (time:int list) (v:int) (d:diffs) : unit =
    Hashtbl.add d time v