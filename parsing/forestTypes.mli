
(* parsing things *)

type var = string
type name = string
type path = string
type aquot = string
type forest_regexp_str = string

type loc = Location.t
val pp_loc : Format.formatter -> Location.t -> unit




(** These are file paths, variable names, quoted OCaml code, and types used to
    generate regular expressions respectively *)

type fPayload = PNone | PVList of var list | PRec

(** This type represents the possible payloads forest AST nodes can
    carry. [PVList] is used to log dependencies while [PRec] marks certain
    recursive ASTs. *)

type 'a ast = { node : 'a; payload : fPayload; loc : loc; }

val mk_ast : loc -> 'a -> 'a ast
val mk_p_ast : loc -> fPayload -> 'a -> 'a ast


(* forest types for a full ast *)





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
  | InVar of loc * var


type specification =
  | File
  | Pads of var
  | PathExp of exp * specification ast
  | Comp of specification ast * var * exp
  | DPair of var * specification ast * specification ast
  | Var of var

(* Generated functions *)

val show_fPayload: fPayload -> string
val show_ast: (Format.formatter -> 'a -> unit) -> ('a ast) -> string
val show_var: var -> string
val show_name: name -> string
val show_path: path -> string
val show_aquot: aquot -> string
val show_forest_regexp_str: forest_regexp_str -> string
val show_exp: exp -> string
val show_specification: specification -> string





