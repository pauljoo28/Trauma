open Collection
open Diffs

type trace = (string, Diffs.diffs) Hashtbl.t

let empty : trace = Hashtbl.create 10000

let get (key:string) (tr:trace) : diffs =
    Hashtbl.find tr key

let populate_helper (col:collection) (tr:trace) (time:int list) : unit =
    Collection.iter (fun k v -> Diffs.insert time v (get k tr)) col

let populate (cols:collection list) (tr:trace) : unit =
    match cols with
    | [] -> ()
    | h :: t ->