
(* parsing types *)
type var = string [@@deriving show]
type name = string [@@deriving show]
type path = string [@@deriving show]
type aquot = string [@@deriving show]
type forest_regexp_str = string [@@deriving show]

type loc = Location.t
let pp_loc = Location.print_loc


type fPayload =
  | PNone
  | PVList of var list
  | PRec [@@deriving show]

type 'a ast =
  { node : 'a;
    payload : fPayload;
    loc : loc;
  } [@@deriving show]


(* forest types *)



type exp =
  | EmptyExp of loc
  | Var of loc * var
  | Name of loc * name
  | Data of loc * string
  | List of loc * exp list
  | DirList of loc * (string -> bool)
  | VDirList of loc * (var * (string -> (string -> bool)))
  | Matches of loc * forest_regexp_str
  | InList of loc * aquot
  | InVar of loc * var [@@deriving show]


type specification =
  | File
  | Pads of var
  | PathExp of exp * specification ast
  | Comp of specification ast * var * exp
  | DPair of var * specification ast * specification ast
  | Var of var [@@deriving show]



let mk_p_ast (loc : loc) (payload : fPayload) (node : 'a) : 'a ast =
  { node; loc; payload}

let mk_ast (loc : loc) (node : 'a) : 'a ast = mk_p_ast loc PNone node