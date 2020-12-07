open Assoc
open ImpAST

type store = int Assoc.context
type fstore = expr Assoc.context
type cstore = expr Assoc.context

type sigma = store * fstore * cstore

let empty = (Assoc.empty, Assoc.empty, Assoc.empty)

let getstore ((store, fstore, cstore) : sigma) : store = store
let getfstore ((store, fstore, cstore) : sigma) : fstore = fstore
let getcstore ((store, fstore, cstore) : sigma) : cstore = cstore

let updatestore (v:string) (x:int) ((store, fstore, cstore) : sigma) : sigma =
  (Assoc.update v x store, fstore, cstore)
let updatefstore (v:string) (c:expr) ((store, fstore, cstore) : sigma) : sigma =
  (store, Assoc.update v c fstore, cstore)
let updatecstore (v:string) (c:expr) ((store, fstore, cstore) : sigma) : sigma =
  if Assoc.mem v store then failwith "Cannot update the expr store, use another variable"
  else (store, fstore, Assoc.update v c cstore)

let lookupstore (v:string) ((store, fstore, cstore) : sigma) : int =
  Assoc.lookup v store
let lookupfstore (v:string) ((store, fstore, cstore) : sigma) : expr =
  Assoc.lookup v fstore
let lookupcstore (v:string) ((store, fstore, cstore) : sigma) : expr =
  Assoc.lookup v cstore

let memstore (v:string) ((store, fstore, cstore) : sigma) : bool =
  Assoc.mem v store
let memfstore (v:string) ((store, fstore, cstore) : sigma) : bool =
  Assoc.mem v fstore
let memcstore (v:string) ((store, fstore, cstore) : sigma) : bool =
  Assoc.mem v cstore