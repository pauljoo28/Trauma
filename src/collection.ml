type collection = (string, int) Hashtbl.t

let empty : collection = Hashtbl.create 10000

let get (k:'a) (c:collection) : int = 
    Hashtbl.find c k

let iter (f:('a -> int -> unit)) (h:collection) : unit = 
    Hashtbl.iter f h

let insert (c:collection) (key:'a) (n:int) : unit =
    Hashtbl.add c key n

let add (c1:collection) (c2:collection) : unit =
    Hashtbl.iter (fun x y -> Hashtbl.add c1 x (Hashtbl.find c1 x + y)) c2