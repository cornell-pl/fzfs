(* Abstract type to represent a zipper file system *)

(* TODO: This should clearly be a Monad... *)
type t

type node_type =
  | ZFSDir of string list
  | ZFSFile of string

type fsErr =
  | CommitFail
  | IllegalAction of string

type t_or_fail = (t,fsErr) Core.result

type path = string
type name = string
type data = string

(* Connect to zipper file system server (identified by [host] and
   [port]) and get a local replica, start a new implicit transaction. *)
val create : ?port:int -> ?host:string -> unit -> t

(* Disconnect from the zipper file system server, attempting to commit the implicit
   transaction. Returns a bool indicating if the commit was successful *)
val destroy : t -> bool

(* Disconnect from the zipper file system server without committing your changes *)
val destroy_no_commit : t -> unit

(* Creates a new ZFS, and runs 'f' until it successfully commits. *)
val run_txn : ?port:int -> ?host:string -> f:(t -> t) -> unit -> unit

(* Standard zipper navigation operations *)
val up   : t -> t_or_fail
val down : t -> t_or_fail
val next : t -> t_or_fail
val prev : t -> t_or_fail

(* [goto p] navigates to path [p], formatted as "/a/b/c". Paths
   containing ".." and "." are suppported *)
val goto : path -> t -> t_or_fail

(* [go_do_return ~f p] navigates to path [p], formatted as "/a/b/c", runs
   f and returns to its original path before returning the relevant data. *)
val go_do_return : f:(t -> ('a * t,fsErr) Core.result) -> path -> t -> ('a * t,fsErr) Core.result


(* [goto n] navigates to child [n] *)
val goto_child : name -> t -> t_or_fail

(* [update data] sets the contents of the file in focus to [data].
   Precondition: the zipper must have a file in focus. *)
val update : data -> t -> t_or_fail

(* [remove_child n] removes the child named [n].
   Precondition: the zipper must have a directory with child n in focus. *)
val remove_child : name -> t -> t_or_fail

(* [mkdir n] adds a directory named [n].
   Precondition: the zipper must have a directory *without* child n in focus. *)
val mkdir : name -> t -> t_or_fail

(* [mkfile n] adds a file named [n].
   Precondition: the zipper must have a directory *without* child n in focus. *)
val mkfile : name -> t -> t_or_fail

val fetch : t -> node_type
val has_parent : t -> bool

(* [is_dir z] is equivalent to `match fetch z with ZFSDir _ -> true | _ -> false` *)
val is_dir : t -> bool

(* [has_child z n] is equivalent to
   `match fetch z with ZFSDir l -> List.mem l n | _ -> false` *)
val has_child : name -> t -> bool


(* [get_path z] gets the current working path *)
val get_path : t -> path

(* [get_global_path z] gets the path of the client global underling zfs
 * rather than the working zfs*)
val get_global_path : t -> path

