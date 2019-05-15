open Core
open DTZUtils
open Async
open Async_parallel_deprecated

val set_debug : unit -> unit
       
type hostname = string 
                  
module Path = String
                                             
module type LeafType = sig
  type leafType
  val to_string : leafType -> string
end

module Make(LeafType : LeafType) : sig
  type pathTree =
    | PathNode of pathTree Path.Map.t
    | PathLeaf of (Path.t * hostname)
                    
  module Connection : sig
    type t
    and tree =
    | Node of tree String.Map.t
    | Leaf of LeafType.leafType
    | Shard of t
         
    val run: t -> f:(tree -> 'b) -> 'b
    (* Add more functions like map and fold *)
    val explore: t  -> unit Deferred.t
  end              
  type tree = Connection.tree 
  type parser = Path.t -> tree

  (**
   * PathDesc uses a pathTree and a parser from paths to trees to 
   * describe the resulting tree.
   *
   * RealDesc uses a tree, the name of its root, and a list of servers to describe
   * and distribute the given tree
   *)                            
  type description =
    | PathDesc of (pathTree * parser)
    | RealDesc of (tree * string)
                    
  module Zipper : sig
    type t
    val show:  t -> string
    
    (** The navigation functions let you move around in the zipper. 
     *  Goto takes the name of a child to traverse to
     *)
    val up: t -> t
    val goto: string -> t -> t
    val down: t -> t
    val next: t -> t
    val prev: t -> t
                     
    val update: tree -> t -> t
  end

  val empty_command: ?summary : string -> (unit -> unit) Core.Command.Param.t -> Core.Command.t
                        
  val distribute: description -> Connection.t Deferred.t
                        
  val start: Core.Command.t -> hostname list -> unit
  val shutdown: unit -> unit
end
