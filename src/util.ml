open Assoc
open ImpAST

type store = int Assoc.context

type fstore = expr Assoc.context

type sigma = store * fstore

let empty = (Assoc.empty, Assoc.empty)

let getstore ((store, fstore) : sigma) : store = store
let getfstore ((store, fstore) : sigma) : fstore = fstore

let updatestore (v:string) (x:int) ((store, fstore) : sigma) : sigma =
  (Assoc.update v x store, fstore)
let updatefstore (v:string) (c:expr) ((store, fstore) : sigma) : sigma =
  (store, Assoc.update v c fstore)

let lookupstore (v:string) ((store, fstore) : sigma) : int =
  Assoc.lookup v store
let lookupfstore (v:string) ((store, fstore) : sigma) : expr =
  Assoc.lookup v fstore

let memstore (v:string) ((store, fstore) : sigma) : bool =
  Assoc.mem v store
let memfstore (v:string) ((store, fstore) : sigma) : bool =
  Assoc.mem v fstore