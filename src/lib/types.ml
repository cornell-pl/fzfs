(* TODOS: 
 * - Abstract leafType by placing in module 
 *)

(* Primitives *)
type name = string [@@deriving show]
type data = string [@@deriving show]

type path =
 | Name of name
 | CDir
 | PDir
 | Root
 | PConcat of path * path
 [@@deriving show]

(* Filesystem type *)
type fsTree =
  | Dir of (name * fsTree) list
  | File of string
  | Unforced

type fsErr =
  | CommitFail
  | IllegalAction of string

type fetchRes =
  | FDir of name list
  | FFile of string
   [@@deriving show]

type fsZipper = 
  { ancestor : fsZipper option;
    left : (name * fsTree) list;
    current : (name * fsTree);
    right : (name * fsTree) list;
  }

type fsCommand = 
  | Up | Down | Next | Prev
  | Goto of path | GotoChild of name | GoDoReturn of path * fsCommand
  | Update of data | RemoveChild of name | AddChild of name * fetchRes

  | Skip | Seq of fsCommand * fsCommand

  | Inspect | Commit
  [@@deriving show]

type fsMessage =
  | FSErr of fsErr
  | FSZip of fsZipper