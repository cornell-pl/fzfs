open Core


type name = string
type path = string

module Var = String
type var = Var.t

type exp =
  | EmptyExp
  | Var of var
  | Name of name
  | Data of string
  | List of exp list
  | DirList of (string -> bool)
  | VDirList of (var * (string -> (string -> bool)))


type specification =
  | File
  | PathExp of exp * specification
  | IPair of specification * specification
  | Comp of specification * var * exp
  | DPair of var * specification * specification


type fetch_md = path  [@@deriving show]

type fetch_info =
  | FileRep of string
  | PathRep
  | PairRep
  | CompRep of (string list) [@@deriving show]

type fetch_result = fetch_md * fetch_info  [@@deriving show]


type contract
type t

type t_or_fail = (t,string) Core.result

(* Helper functions *)

val set_debug : unit -> unit
val p : ('a, Stdio.Out_channel.t, unit) format -> 'a
val d : ('a, unit, string, unit) format4 -> 'a

val print_fetch_result : fetch_result -> unit

(* [print t] fetches the current node and prints the result *)
val print : t -> unit
val print_with_newline : t -> unit

(* [debug_print t] prints the entire zipper structure *)
val debug_print : t -> unit

val mk_err : ('a, unit, string, t_or_fail) format4 -> 'a
val mk_ok : 'a -> ('a,string) Core.result

val sync_path: t -> t_or_fail

(* Main Functions *)

(* [mk_zipper s p] creates a zipper rooted at p from s.
 * If p is an invalid path it returns an error. *)
val mk_zipper : specification -> path -> t_or_fail

(* [finish t] removes the filesystem connection and tries to commit returning
whether it was successful or not *)
val finish : t -> bool

(* These are standard zipper navigation operations that may fail
 * based on the current focus
 * these each sync zfs with fs *)
val up : t -> t_or_fail
val down : t -> t_or_fail
val next : t -> t_or_fail
val prev : t -> t_or_fail

(* [goto s t] goes the name s in the comprehension t
 * syncs zfs with fs*)
val goto : string -> t -> t_or_fail

(* [fetch t] returns a fetch_result representing the current node
 * syncs zfs with fs*)
val fetch : t -> fetch_result

(* [shallow_fetch t] returns a fetch_result representing the current node
 * this does not recursivly fetch what comes at then end of a path
 * syncs zfs with fs*)
val shallow_fetch : t -> fetch_result

(* [get_forest_path t] returns the current working path of the forest
 * zipper *)
 val get_forest_path: t -> path

(*delete the unsynroized path*)
(* [get_zfs_path z] returns the path of the global zfs path for this
 * client (i.e. the last path that the client manuved to)*)
 val get_zfs_path: t -> path


(* [put n t] tries to replace the current node with an interpretation of n,
 * but fails if the type of n does not correspond to t's focus
 * syncs zfs with fs*)
val put : fetch_info -> t -> t_or_fail

(* [run_txn ~f s p ()] creates a new zipper from s at path p,
 * runs f on that zipper, then tries to commit, rerunning as necessary until
 * it succeeds. WARNING: This can loop infinitely if given a function that
 * can never successfully commit *)
val run_txn : f:(t -> t) -> specification -> path -> unit -> unit

(* [run_txn_exn] works as [run_txn], except that users are not required to
 * handle failures, and the function will instead throw an exception if they
 * occur. *)
 val run_txn_exn : f:(t -> t_or_fail) -> specification -> path -> unit -> unit

(* [check s t] checks if operation 's' is available in t *)
val check : string -> t -> bool

(*[map_reduce_postorder m f init t] returns a pair (a, t') where a is the result
 * of chaning together applications of f to all the nodes in the zipper t. t' is
 * a new zipper where m has been applied to each node in the zipper. In
 * this version, f and m will be applied at the children of a node and then the
 * node it self, they will be applied to the children of a node in the order
 * they are in the zipper. m is applied before f.
 *  - preconditon : for any init and t, if f init t moves the zipper
 *                  it must move it back to the original position
 *)
val map_reduce_postorder : m:(t -> t_or_fail) -> f:('a -> t -> 'a) -> init:('a) -> t -> 'a * t_or_fail

(*[map_postorder m t] applies m to all nodes in the zipper, constructing a new
 * zipper. m is applied in the same order as in map_reduce_postorder*)
val map_postorder : m:(t -> t_or_fail) -> t -> t_or_fail

(*[fold_postorder f init t] applies f to all nodes in the zipper, starting with
 * an initialization of init and aggegating nodes into a result.
 * f is applied in the same order as in map_reduce_postorder
 *  - preconditon : for any init and t, if f init t moves the zipper
 *                  it must move it back to the original position*)
val fold_postorder : f:('a -> t -> 'a) -> 'a -> t -> 'a

(*[map_reduce_preorder m f init t] returns a pair (a, t') where a is the result
 * of chaning together applications of f to all the nodes in the zipper t. t' is
 * a new zipper where m has been applied to each node in the zipper. In
 * this version, f and m will be applied to a node and then its children.
 * m and f will be applied to the children of a node in the order they are in
 * the zipper. m is applied before f.
 *  - preconditon : for any init and t, if f init t moves the zipper
 *                  it must move it back to the original position
 *)
val map_reduce_preorder : m:(t -> t_or_fail) -> f:('a -> t -> 'a) -> init:('a) -> t -> 'a * t_or_fail

(*[map_postorder m t] applies m to all nodes in the zipper, constructing a new
 * zipper. m is applied in the same order as in map_reduce_preorder*)
val map_preorder : m:(t -> t_or_fail) -> t -> t_or_fail

(*[fold_postorder f init t] applies f to all nodes in the zipper, starting with
 * an initialization of init and aggegating nodes into a result.
 * f is applied in the same order as in map_reduce_preorder
 *  - preconditon : for any init and t, if f init t moves the zipper
 *                  it must move it back to the original position
 *)
val fold_preorder : f:('a -> t -> 'a) -> 'a -> t -> 'a
