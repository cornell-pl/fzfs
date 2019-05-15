open Core
open DTZUtils

(** Global information and functions *)
  
let globalVersion = ref 0
let globalLog : String.Set.t Int.Table.t = Int.Table.create ()
let globalMutex = Mutex.create ()

let getEntries start finish =
  Hashtbl.filter_keys globalLog ~f:(fun version -> version >= start && version <= finish)
  |> Hashtbl.data 
  |> String.Set.union_list


(** Transactional modules and types *)
      
(*
 * Transactions have two interesting operations for concurrency purposes:
 * - Writes on paths
 * - Reads on paths
 * And for extra information, we also store a list of commands
 * Also we need a method of commiting and refreshing
 *)
  
module type Transaction = sig
  type logs
  include Monad.S
  val create : unit -> logs
  val runState : 'a t -> logs -> ('a * logs)
  val write : ('a * path) t -> 'a t
  val read : ('a * path) t -> 'a t
  val add_command : path -> string -> 'a t -> 'a t
  val commit
    : writer: Async.Writer.t
    -> id:Async.Writer.Id.t
    -> run
       :( writer: Async.Writer.t
       -> id: Async.Writer.Id.t
       -> string
       -> 'a t
       -> 'a t
       )
    -> update:('a -> unit)
    -> 'a ref
    -> 'a t
    -> 'a t
  val refresh : 'a -> 'a t
end
  
module OptimisticTransaction : Transaction = struct
    type logs = 
      {
        writeLog : String.Set.t;
        readLog : String.Set.t;
        commandList : (path * string) list;
        version : int;
      }
        
    (** Monadic parts *)
    type 'a t = logs -> 'a * logs

    let set x logs = x, logs
  
    include Monad.Make(struct
      type nonrec 'a t = 'a t
      let bind m ~f logs =
        let (x,logs) = m logs in
        f x logs (* 'a -> logs -> logs -> 'b * logs *)
          
      let return = set

      let map = `Define_using_bind
    end)
      
    let runState m logs = m logs

    (** Log related stuff *)
    (** Helpers *)
    let create () =
      {
        writeLog = String.Set.empty;
        readLog = String.Set.empty;
        commandList = [];
        version = !globalVersion;
      }
        
    (* TODO: 
       1. May need to deal with mutex better here 
       2. We wanna fix this to check prefixes instead of just normal set intersection
    *)
    let check_log =
      bind ~f:(fun _ logs ->
        let fresh () =
          globalVersion := !globalVersion + 1;
          !globalVersion
        in
        let written_since_refresh = getEntries logs.version (fresh ()) in
        let ok = String.Set.inter written_since_refresh logs.readLog |> String.Set.is_empty in
        ok,logs
      )

    let editCommandList = ["echo";"touch";"mkdir";"cp";"rm";"mv"]

    let clear_commands = bind ~f:(fun x logs -> x,{logs with commandList = []})

    let get_commands = bind ~f:(fun _ logs -> List.rev logs.commandList,logs)

    let replay_commands ~run ~writer ~id m =
      get_commands m >>= fun commands ->
      let replay_command (m : 'a t) (command : string) : 'a t =
        try 
          run ~writer ~id command m
        with Failure(s) ->
          write_response id writer @@ Error(s);
          m
      in
      let preprocess_commands commands =
        List.map commands ~f:(fun (path,command) -> ["cd " ^ path;command])
                |> List.concat
      in 
      List.fold ~init:m ~f:replay_command (preprocess_commands commands)
      
    (** Operation interface *)
    let write = bind ~f:(fun (x,path) logs -> x,{logs with writeLog = Set.add logs.writeLog path})
      
    let read = bind ~f:(fun (x,path) logs -> x,{logs with readLog = Set.add logs.readLog path})
      
    let add_command path command =
      match String.split ~on:' ' command with
      | [] -> failwith "Got empty input: %s" command
      | hd :: tl ->
         if List.exists ~f:(String.equal hd) editCommandList
         then bind ~f:(fun x logs -> x,{logs with commandList = (path,command) :: logs.commandList})
         else Fn.id


    (* TODO: May wish to do something more clever with the log, but atm we 
       want to be conservative since we would need to deal with possible changes that have
       already been made, but we wouldn't see if we made a new log with a new version.
       Could autorefresh?
    *)
    let commit ~writer ~id ~run ~update globalState m =
      let update_global = bind ~f:(fun x logs ->
        Hashtbl.set globalLog ~key:!globalVersion ~data:logs.writeLog;
        x,logs)
      in
      Mutex.critical_section globalMutex
        ~f:(fun () ->
          check_log m >>= fun b ->
          if b
          then
            begin
              update_global m
              >>= fun _ -> replay_commands ~run ~writer ~id (return !globalState)
              >>| (fun zipper -> update zipper)
              >>= fun _ ->
              write_response id writer @@ Message("Commit successful!");
              clear_commands m
            end
          else
            begin
              write_response id writer @@ Message("Commit failed: Refresh and try again");
              m
            end
        )

    let refresh x _ = set x (create ())   
end
  
