open Core
open DTZUtils
open Async
open Async_parallel_deprecated

let debug = ref false
let set_debug () = debug := true
                
type hostname = string 

module Path = String

                
let p format = Core.Printf.ksprintf (Core.Printf.printf "%s: %s\n%!" (Pid.to_string (Unix.getpid ()))) format

let d format = if !debug then p format else Core.Printf.ksprintf ignore format
                                 
module type LeafType = sig
  type leafType
  val to_string : leafType -> string
end                                            

module Make (LeafType : LeafType) = struct
  include LeafType
            
  type pathTree =
    | PathNode of pathTree Path.Map.t
    | PathLeaf of (Path.t * hostname)

  module Connection = struct
    type t = (communicationCommand,communicationResponse) Channel.t
    and tree =
    | Node of tree String.Map.t
    | Leaf of leafType
    | Shard of t
                                                         
    and communicationCommand =
      | Explore of string
        
    and communicationResponse =
      | ExploreResponse of t option * string
      | ExploreError of string
           

    let run connection ~f =
      failwith "TODO: Run not implemented yet"


    let explore_write connection input = Channel.write connection (Explore input)
                                                       
    let explore_read connection =
      match%map Channel.read connection with
      | ExploreResponse (None,response) -> connection,response
      | ExploreResponse (Some connection,response) -> connection,response
      | ExploreError (error) -> connection,"Error: " ^ error ^ "\n"
        
      
    let explore connection : unit Deferred.t =
      let client_reader = Core.force Reader.stdin in
      let client_writer = Core.force Writer.stdout in
      let quit_flag = ref false in
      let rec client_handler () =
        let%bind _,response = explore_read connection in
        if !quit_flag
        then return ()
        else
          begin
            Writer.write client_writer response;
            client_handler ()
          end
      in
      let rec server_handler () =
        read_and_run client_reader ~debug:ignore
          ~f:(fun input ->
            explore_write connection input;
            if String.is_prefix ~prefix:"quit" input
            then
              begin
                quit_flag := true;
                return ()
              end
            else
              server_handler ()
          )
      in
      try
        let ch = client_handler () in
        let sh = server_handler () in
        Deferred.both ch sh
        >>| ignore              
      with
      | Failure(s) ->
         d "Failure: %s" s;
         return ()
      | _ ->
         d "Unknown Error";
         return ()
        (*
      explore connection >>> fun () -> () *)

    let make channel = channel
  end

  type tree = Connection.tree 
  type parser = Path.t -> tree
                                             
  type description =
    | PathDesc of (pathTree * parser)
    | RealDesc of (tree * string)

                        
  module Zipper = struct
    type t = {current: tree; parent: parent; name: string}

    and parent =
      | Zipper of t
      | Hub of (Connection.communicationCommand,Connection.communicationResponse) Hub.t
      | Empty

    let make_hub hub = Hub hub
    let make ?(parent=Empty) ~node name = {current = node; parent; name}
                                    
    let of_map ?(parent=Empty): tree String.Map.t -> t list =
      Map.fold ~init:[] ~f:(fun ~key ~data acc -> make ~parent ~node:data key :: acc)

    
               
    let show zipper =
      match zipper.current with
      | Leaf content -> to_string content
      | Node map ->
         Map.to_alist map
         |> List.map ~f:(fun (name,data) ->
                match data with
                | Leaf _ -> name ^ "\n"
                | _ -> name ^ "/\n"
              )
         |> String.concat ~sep:""
      | Shard _ -> failwith "ls: Shards should never be the current term"
                            
    (** Helper functions *)
    let fix_name newName name = if name = "" then newName else name
                                                                 
    let root_name = fix_name "/"
                             
    let transform_name zipper = root_name zipper.name
                                          
    let path_from_zipper zipper =
      let rec get_path zipper =
        match zipper.parent with
        | Zipper (z) -> get_path z ^/ zipper.name
        | _ -> zipper.name
      in
      get_path zipper
               
    let make_path zipper name = (path_from_zipper zipper) ^/ name
                                                               
    (** Navigation functions *)
                                                               

    let get_folder = function
      | {current = Node map; _ } -> map
      | {current = Shard _; name; _ } -> failwith "%s is a Shard, so something went wrong" name
      | {name; _} -> failwith "%s is not a folder" name
                              
    let up zipper =
      (* TODO (jdl): Fix this and support shards *)
      match zipper.parent with
      | Empty -> failwith "%s has no parents" zipper.name
      | Hub _ -> failwith "Hubs aren't currently supported"
      | Zipper z ->
         match z.current with
         | Node map -> {z with current = Node (Map.set map ~key:zipper.name ~data:zipper.current)}
         | _ -> failwith "Something has gone horribly wrong: Parent should always be a node"

    (* TODO (jdl): Goto, down, and move_in are too similar. 
     * Figure out a way to factor out the similarities *)
    let goto name zipper =
      let errorString = Printf.sprintf "%s does not exist in %s" name (transform_name zipper) in
      let result = Map.find (get_folder zipper) name in
      let zipper =
        Option.map result ~f:(fun node -> {name;current = node;parent = Zipper zipper})
      in
      value_or_fail errorString zipper
                    
    let down zipper =
      let errorString = Printf.sprintf "Folder %s is empty" (transform_name zipper) in
      let result = Map.min_elt (get_folder zipper) in
      let zipper =
        Option.map result ~f:(fun (name,node) -> {name;current = node;parent = Zipper zipper})
      in
      value_or_fail errorString zipper

    let move_in ~direction zipper =
      let parent = up zipper in
      let errorString =
        let direction =
          match direction with
          | `Greater_than -> "after"
          | `Less_than -> "before"
          | _ -> failwith "Illegal direction passed to internal function move_in"
        in
        Printf.sprintf "No path %s %s in %s" direction zipper.name (root_name parent.name)
      in
      let result = Map.closest_key (get_folder parent) direction zipper.name in
      let zipper =
        Option.map result ~f:(fun (name,node) -> {name; current = node; parent = Zipper parent})
      in
      value_or_fail errorString zipper
                    
    let next zipper = move_in ~direction:`Greater_than zipper
                              
    let prev zipper = move_in ~direction:`Less_than zipper

    let update current zipper = {zipper with current}

  end
                    
  open Zipper
  open Connection
  module OTM = TransactionMonad.OptimisticTransaction
                 
  let rec fscommands ~hub ~id =
    let help = help ~hub ~id in
    let show zipper =
      Hub.send hub id (ExploreResponse(None,Zipper.show zipper));
      zipper
    in
    let goto = function
      | path :: [] -> OTM.map ~f:(goto path)
      | [] -> failwith "Invalid argument (empty)\nUsage: <cmd> path"
      | _ as args ->
         failwith "Invalid argument: %s\nUsage: <cmd> path" (String.concat ~sep:" " args)
    in
    [
      "show", (fun _ -> OTM.map ~f:show);
      "prev", (fun _ -> OTM.map ~f:prev);
      "next", (fun _ -> OTM.map ~f:next);
      "up", (fun _ -> OTM.map ~f:up);
      "down", (fun _ -> OTM.map ~f:down);
      "goto", goto;
      "help", help;
      "quit", (fun _ m -> m);
    ]
      
  and help ~hub ~id _ monad =
    let help_string =
      "Commands: " ^ (String.concat ~sep:", " (List.map ~f:fst (fscommands ~hub ~id))) ^ "\n"
    in
    ExploreResponse(None,help_string) |> Hub.send hub id;
    monad

  and perform_command ~hub ~id input =
    match String.split ~on:' ' input with
    | [] -> failwith "Got empty input: %s" input
    | command :: rest ->
       match List.Assoc.find ~equal:String.equal (fscommands ~hub ~id) command with
       | None -> failwith "Command `%s` does not exist" command
       | Some(f) -> f rest

  let write_to_hub ~id hub = Hub.send hub id                       
  let write_response ~id hub message = write_to_hub ~id hub (ExploreResponse(None,message))
  let write_error ~id hub message = write_to_hub ~id hub (ExploreError(message))

  let print_prompt ~id hub zipper =
    let prompt = (Zipper.make_path zipper ">") ^ " " in
    write_response ~id hub prompt

  let master_setup root hub =
    let write_error = write_error hub in
    let globalZipper = ref root in
    Pipe.fold' (Hub.listen hub) ~init:(!globalZipper,OTM.create ())
      ~f:(fun ziplogs queue ->
        Queue.fold ~init:ziplogs queue ~f:(fun (zipper,logs) -> function
            | `Connect id ->
               d "Client %d connected" (Obj.magic (Hub.Client_id.hash id));
               print_prompt ~id hub zipper;
               zipper,logs
            | `Disconnect (id,msg) ->
               d "Client %d disconnected with msg: %s" (Obj.magic (Hub.Client_id.hash id)) msg;
               zipper,logs
            | `Data (id,Explore command) ->
               try 
                 let (zipper,logs) = 
                   perform_command ~hub ~id command (OTM.return zipper)
                   |> (Fn.flip OTM.runState) logs
                 in
                 print_prompt ~id hub zipper;
                 zipper,logs
               with
               | Failure(s) ->
                  write_error ~id s;
                  print_prompt ~id hub zipper;
                  zipper,logs
          )
        |> return
      )


  module TreeOperations = struct
    let split_to_zipper tree rootName =
      match tree with
      | Node map ->
         let newRoot,rest = Map.partition_tf map (function | Node _ -> false | _ -> true) in
         (* TODO (JDL): Remove this if you don't need it
         let newRoot =
           Map.merge newRoot rest ~f:(fun ~key -> function
               | `Both _ -> failwith "Partitions can't have the same key"
               | `Left v -> Some(v)
               | `Right _ -> Some(Shard )
             )
         in
          *)
         let main_zipper = Zipper.make ~node:(Node newRoot) rootName in
         main_zipper, Zipper.of_map map
      | Leaf _ -> Zipper.make ~node:tree rootName,[]
      | _ -> failwith "Shards should not be used by users"
  end

  open TreeOperations
  open Std
  open Zipper
         
  let empty_command ?(summary="Default Summary") main =  Core.Command.basic ~summary main
                                                                            
  (* TODO (JDL): Uncomment
  let spawn_and_connect root rest =
    let worker_spawn i hub =
      let globalZipper = ref (List.nth_exn rest i) in
      failwith "TODO"
    in
    let master_spawn hub =
      let%map children =
        List.mapi rest ~f:(fun i child ->
            Parallel.spawn ~where:`Local (worker_spawn i) >>|
              fun (channel,_) -> (channel,child)
          )
        |> Deferred.all
      in
      let newRoot =
        match root.current with
        | Node map -> 
           let map =
             List.fold children ~init:map
               ~f:(fun map (channel,child) -> Map.set ~key:child.name ~data:(Shard channel) map)
           in
           {root with current = Node map}
        | _ -> root
      in
      let globalZipper = ref newRoot in
      failwith "TODO"
    in
   *)
                                                                            
  let assign_to_servers = List.map ~f:(fun zipper -> (zipper,Parallel.round_robin))

  let distribute description =
    let master_spawn root hub =
      master_setup root hub
    in

    match description with
    | RealDesc (tree,name) ->
       let root,rest = split_to_zipper tree name in
       (* TODO (JDL): REMOVE THIS LINE *)
       let root = Zipper.make ~node:tree name in
       let%map (masterChannel,_) = Parallel.spawn ~where:`Local (master_spawn root) in
       Connection.make masterChannel
    | PathDesc (pathTree,parser) ->
       (* Nodes -> spawn a worker with a zipper with whatever children but no content 
        * Leafs -> spawn a worker which makes a leaf zipper with appropriate parent and parses info
        *)
       
       let rec build_tree ?parent name pathTree =
         match pathTree with
         | PathLeaf (path, host) ->
            let spawn_leaf hub =
              let zipper = Zipper.make ?parent ~node:(parser path) name in
              master_setup zipper hub
            in
            let%map (masterChannel,_) = Parallel.spawn ~where:(`On host) spawn_leaf in
            Connection.make masterChannel
         | PathNode map ->
            let spawn_node hub =
              let%map map =
                Map.to_alist map
                |> List.map ~f:(fun (key,data) ->
                       build_tree ~parent:(Zipper.make_hub hub) key data
                       >>| fun channel -> (key,Shard channel))
                |> Deferred.all
                >>| String.Map.of_alist_exn
              in
              let zipper = Zipper.make ?parent ~node:(Node map) name in
              master_setup zipper hub
            in
            let%map (masterChannel,_) = Parallel.spawn ~where:`Local spawn_node in
            Connection.make masterChannel
      in
      build_tree "/" pathTree

                       
  (* TODO (jdl): Consider if we want to allow configurations where this machine isn't included *)
  let start command worker_machines =
    Parallel.init ~cluster:{master_machine = Unix.gethostname (); worker_machines} ();
    Command.run command;
    never_returns (Scheduler.go ())

  let shutdown () =
    let force =
      match%map Parallel.shutdown () with
      | Ok () -> d "Shutdown successful"
      | Error e -> p "Shutdown error: %s" (Error.to_string_hum e)
    in
    force >>> fun () -> Shutdown.shutdown 0

end

                                      
     
(*    let open Connection in
    let%bind worker =
      RPCMaster.spawn
        ~on_failure:Error.raise
        ~shutdown_on:Disconnect
        ~connection_state_init_arg:()
        ~name:rootName
        rootNode
    in
    (* TODO (jdl): YOU ARE HERE! Decide if you wanna use Async Parallel even
     * though it is deprecated, or just figure out some minimum thing necessary to use this
     *)
     *)  

  (** You already have a bunch of master threads on different shards. 
Now you want to distribute the data to run on it.
   *)
(*
open ZfsUtils       
type current_host =
  | Master
  | Slave of Async.Writer.t
     
type navigation =
  | Update of term
  | DownTo of fileName
  | Up
  | Next of bool (*Forward = true, Backward = false*)
  | Down

type parent =
  {
    name : fileName;
    term : term;
    updated : bool;
  }

type zipper =
  {
    current : parent;
    parents : parent list;
  } 

type marshalable =
  | MFile of fileContent
  | MFolder of (fileName * marshalable) list
    
let rec make_marshalable = function
  | File c -> MFile c
  | Folder map ->
     let mmap =
       Folder.fold map ~init:[]
         ~f:(fun ~key ~data accumulator -> (key,make_marshalable data) :: accumulator)
     in
     MFolder mmap
  | Shard _ -> failwith "Shards are not marshalable"

let rec unmarshal = function
  | MFile c -> File c
  | MFolder mmap ->
     let map =
       List.fold mmap ~init:Folder.empty
         ~f:(fun map (key,data) -> Folder.set map ~key ~data:(unmarshal data))
     in
     Folder map
    
module OTM = TransactionMonad.OptimisticTransaction
open OTM

exception PassControl of current_host
       
(** Globals *)
    
let emptyZipper = {current = {name = "/"; term = File ""; updated = false} ; parents = []}
    
let globalZipper = ref emptyZipper

(** Sharding globals *)
  
let idToZiplogs : (Async.Writer.Id.t, (zipper * OTM.logs)) Hashtbl.t = Async.Writer.Id.Table.create ()
let idToWriter : (Async.Writer.Id.t, Async.Writer.t) Hashtbl.t = Async.Writer.Id.Table.create ()
let idToHost : (Async.Writer.Id.t, current_host) Hashtbl.t = Async.Writer.Id.Table.create ()
let shardSet : WriterSet.t = WriterSet.create ()
let commitMutex = Mutex.create ()

(** Helper functions *)
  
let fix_name newName name = if name = "" then newName else name
  
let root_name = fix_name "/"

let here_name = fix_name "Current node"

let transform_name zipper = root_name zipper.current.name

let marshal_node node = Marshal.to_string (make_marshalable node) [Marshal.No_sharing]
let unmarshal_node node = Marshal.from_string node 0 |> unmarshal
  
let get_folder = function
  | {term = Folder(folder); _ } -> folder
  | {term = Shard(_); name; _ } -> failwith "%s is a Shard, so something went wrong" name
  | {name; _} ->   failwith "%s is not a folder" name

let split_parents = function
  | { parents = parent :: parents; _} -> (parent,parents)
  | { current = {name; _}; _} -> failwith "%s has no parents" (root_name name)
     
let get_parent = Fn.compose fst split_parents

let move_helper zipper (name,term) =
  let current = {name; term; updated = false} in
  {current; parents = zipper.current :: zipper.parents}

(** Updates are always propagated to at least one level above where we are (or root) *)
    
let propagate_update_up zipper =
  if zipper.current.updated && zipper.parents <> [] then
    let (parent,parents) = split_parents zipper in
    match parent with
    | {term = Folder(folder); _ } -> 
       let term = Folder (Map.set folder ~key:zipper.current.name ~data:zipper.current.term) in
       let current = {zipper.current with updated = false} in
       {current; parents = { parent with term; updated = true } :: parents}
    | {term = Shard(_); _ } ->
       {zipper with current = {zipper.current with updated = false}}
    | {name; _} ->   failwith "%s is not a folder" name
  else
    zipper

let check_shard_change ~id ~command ~pass zipper =
  match zipper.current.term with
  | Shard writer ->
     write_command id writer (command ());
     if pass
     then raise (PassControl (Slave writer))
     else raise (PassControl Master)
  | _ -> zipper

let no_command () =
  failwith "This move should never fail. Please report sequence of events that caused this failure."
           
let move_to_next ~id from b zipper =
  let direction = if b then `Greater_than else `Less_than in
  let errorString =
    let direction = if b then "after" else "before" in
    Printf.sprintf "No path %s %s in %s" direction from (root_name zipper.current.name)
  in
  let open Option in
  (Map.closest_key (get_folder zipper.current) direction from >>| move_helper zipper)
  |> value_or_fail errorString
  |> check_shard_change ~id ~command:(fun () -> "cd .") ~pass:true
           
let rec move ~id ?(command=no_command) ?(pass=true) navigation zipper =
  let move_down = move_helper zipper in
  let open Option in
  let newZipper =
    match navigation with 
    | Up ->
       let (current, parents) = split_parents zipper in
       propagate_update_up {current; parents}
    | DownTo (name) ->
       let errorString = Printf.sprintf "%s does not exist in %s" name (transform_name zipper) in
       (Map.find (get_folder zipper.current) name >>| fun term -> move_down (name,term))
       |> value_or_fail errorString
    | Down ->
       let errorString = Printf.sprintf "Folder %s is empty" (transform_name zipper) in
       (Map.min_elt (get_folder zipper.current)
        >>| move_down)
       |> value_or_fail errorString
    | Next b ->
       let from = zipper.current.name in
       let (current,parents) = split_parents zipper in
       let zipper = propagate_update_up {current; parents} in
       let command () = if b then "next " ^ from else "prev " ^ from in
       let zipper = check_shard_change ~id ~command ~pass zipper in
       move_to_next ~id from b zipper
    | Update term ->
       let current = { zipper.current with term; updated = true} in
       propagate_update_up {zipper with current}
  in
  check_shard_change ~id ~command ~pass newZipper
       
let path_from_zipper zipper =
  List.map (List.rev (zipper.current :: zipper.parents)) ~f:(fun parent -> parent.name)
  |> Filename.of_parts (*TODO (jdl): Make sure you don't need this: |> (^) "/"*)

let make_path zipper name = (path_from_zipper zipper) ^/ name

let split_path ?default:(default=".") path =
  let open Option in
  let name,path = 
    match String.split ~on:'/' path |> List.rev with
    | [] -> (path, default)
    | name :: [] -> (name,default) (* This should never happen *)
    | name :: rest -> (name, List.rev rest |> String.concat ~sep:"/")
  in
  if name = ".." || name = "."
  then ("", path ^/ name)
  else name,path 

let string_of_path_list = function
  | [] -> "."
  | paths ->
     let path = Filename.of_parts paths in
     if path = "" then "." else path
                      
let moves_with_return ~id ~commandf ?(pass=false) path zipper =
  let rec move_fold navigations (zipper,back) =
    let command () =
      let back,remaining = 
        match navigations with
        | ".." :: remaining ->
           let back = string_of_path_list (zipper.current.name :: back) in
           let remaining = string_of_path_list remaining in
           back,remaining
        | _ :: remaining ->
           let back = string_of_path_list (".." :: back) in
           let remaining = string_of_path_list remaining in
           back,remaining
        | [] ->
          ("",string_of_path_list navigations)
      in
      commandf back remaining
    in
    match navigations with
    | [] -> (zipper,back)
    | ".." :: rest ->
       move_fold rest (move ~id ~command ~pass Up zipper, zipper.current.name :: back)
    | path :: rest ->
       move_fold rest (move ~id ~command ~pass (DownTo path) zipper, ".." :: back)
  in
  let navigations =
    Filename.parts path |> List.filter ~f:(fun path -> not (path = "." || path = ""))
  in
  move_fold navigations (zipper,[]) |> fun (zipper,back) -> zipper, string_of_path_list back
                         
let moves ~id ?(commandf=fun _ -> "") ?(pass=false) path zipper =
  let commandf _ = commandf in (* Ignores back argument *)
  moves_with_return ~id ~commandf ~pass path zipper |> fst

let apply_path1 ~id ~strict f args = 
  match args with
  | path :: [] -> f ~id path
  | [] ->
     if strict
     then failwith "Invalid argument: %s\nUsage: <cmd> path" (String.concat ~sep:" " args)
     else f ~id ""
  | _ -> failwith "Invalid argument: %s\nUsage: <cmd> [path]" (String.concat ~sep:" " args)
     
let apply_path2 ~id f args =
  match args with
  | pathFrom :: pathTo :: [] -> f ~id pathTo pathFrom
  | pathTo :: [] -> f ~id pathTo ""
  | _ -> failwith "Invalid argument: %s\nUsage: <cmd> [from] to " (String.concat ~sep:" " args)

let rec go_to_root ~id zipper =
  match zipper.parents with
  | [] -> zipper
  | _ :: _ -> go_to_root ~id (move ~id ~command:(fun () -> "cd .") Up zipper)
                         
(** Main functions *)
  
let inject_path zipper = OTM.map ~f:(fun x -> (x,path_from_zipper zipper))
                                 
let mknode ~id ?nodeName node path monad =
  let name,path = split_path path in
  let name,nodeName =
    match name,nodeName with
    | "",Some(name) -> name,None
    | _ ->  name,nodeName
  in
  let commandf _ remaining =
    "mknode " ^ remaining ^/ name ^ " " ^ Option.value nodeName ~default:"." ^ " " ^
      (marshal_node node)
  in
  monad
  >>| moves_with_return ~id ~commandf path
  >>| (fun (zipper,back) -> 
    match zipper.current.term with
    | Folder map ->
       begin
         match Map.find map name,nodeName with
         | None, _ -> 
            let newFolder = Folder (Map.set map ~key:name ~data:node) in
            move ~id (Update newFolder) zipper |> moves ~id back,
            make_path zipper name
         | _, None -> failwith "Node %s already exists within %s" name (here_name path)
         | Some(Folder map), Some nodeName ->
            if Map.mem map nodeName
            then failwith "Node %s already exists, as does %s/%s" path path nodeName
            else
              let newFolder = Folder (Map.set map ~key:nodeName ~data:node) in
              let zipper = move ~id (DownTo name) zipper in
              let path = make_path zipper nodeName in
              move ~id (Update newFolder) zipper |> move ~id Up |> moves ~id back, path
         | Some(Shard writer), Some nodeName ->
            write_command id writer ("mknode " ^ nodeName ^ " . " ^ (marshal_node node));
            raise (PassControl Master)
         | Some(File _),_ -> failwith "%s already exists and is a file" name
       end
    | Shard writer ->
       write_command id writer
         ("mknode " ^ name ^ " " ^
            Option.value nodeName ~default:"." ^ " " ^
            (marshal_node node));
       raise (PassControl Master)
    | File _ -> failwith "%s is a file and nodes cannot be created in files" (here_name path)
  ) |> OTM.write
         
let touch = mknode (File "")

let mkdir = mknode (Folder (Folder.empty))
                                 
let echo ~id args monad =
  (* Mostly error checking since it has a super particular format *)
  let extract_path_and_content args =
    let get_content str =
      let string = String.concat ~sep:" " (List.rev str) in
      match String.index_from string 1 '"' with
      | Some(i) when String.length string - 1 = i ->
         if String.get string 0 = '"'
         then String.sub ~pos:1 ~len:(i-1) string
         else failwith "echo: Expected string, got %s" string
      | _ -> failwith "echo: Expected string, got %s" string
    in
    match List.rev args with
    | path :: ">" :: str ->
       path,get_content str
    | _ ->
       failwith
         "echo: invalid argument %s\nUsage: echo \"str\" > filename"
         (String.concat ~sep:" " args)
  in
  (* Actually creating the new file in the proper location *)
  let path,content = extract_path_and_content args in
  let name,dirpath = split_path path in
  let commandf _ remaining = "echo \"" ^ content ^ "\" > " ^ remaining ^/ name in
  monad
  >>| moves_with_return ~id ~commandf dirpath
  >>| (fun (zipper,back) ->
    let path = make_path zipper name in
    match zipper.current.term with
    | Folder map ->
       begin
         match Map.find map name with
         | None 
         | Some(File _) ->
            let newFolder = Folder (Map.set map ~key:name ~data:(File content)) in
            let zipper = move ~id (Update newFolder) zipper in
            moves ~id back zipper, path
         | Some(_) -> failwith "%s is a directory" path
       end
    | _ -> failwith "%s is not a directory" (here_name dirpath)
  ) |> OTM.write
      

(* TODO (jdl): Fix go_to_root to deal with shards appropriately *)
let cd ~id path =
  Hashtbl.set idToHost ~key:id ~data:Master;
  let moves = moves ~id ~commandf:((^) "cd ") ~pass:true in
  OTM.map ~f:(fun zipper ->
      match String.chop_prefix ~prefix:"/" path with
      | Some(path) -> moves path (go_to_root ~id zipper)
      | None -> moves path zipper)

(* TODO (jdl): Currently adding ALL ls's, but that's for sure too conservative *)
let ls ~writer ~id path monad =
  let commandf = (^) "ls " in
  monad >>| moves ~id ~commandf path
  >>= fun zipper ->
    begin
      match zipper.current.term with
      | File content -> write_response id writer @@ File(content)
      | Folder map ->
         let dirlist =
         Map.mapi map
           ~f:(fun ~key ~data ->
             match data with
             | File _ -> F
             | Folder _ -> D
             | Shard shardWriter -> D
           )
         |> Map.to_alist
         in
         write_response id writer @@ Dir(dirlist)
      | Shard _ -> failwith "ls: Shards should never be the current term"
    end;
  OTM.read (inject_path zipper monad)
   
let cp ~id pathTo pathFrom monad =
  let commandf back remaining = "cp " ^ remaining ^ " " ^ back ^/ pathTo in
  monad >>| moves_with_return ~id ~commandf pathFrom
  >>= fun (tmpZipper,_) ->
  match tmpZipper.parents with
  | {term = Shard _;_} :: _ ->
     failwith "cp: %s is a shard. Shards can't be copied." tmpZipper.current.name
  | _ ->
     OTM.read (inject_path tmpZipper monad)
     |> mknode ~id ~nodeName:tmpZipper.current.name tmpZipper.current.term pathTo 

let rm ~id path monad =
  let commandf _ = (^) "rm " in
  monad
  >>| moves_with_return ~id ~commandf path
  >>| (fun (zipper,back) -> 
    let path = path_from_zipper zipper in
    let back =
      Filename.parts back |> List.tl_exn |> List.tl
      |> Option.value ~default:[] |> string_of_path_list
    in
    let name = zipper.current.name in
    let zipper = move ~id Up zipper in
    (* TODO (jdl): Need to save zipper here *)
    match zipper.current.term with
    | Folder map -> move ~id (Update (Folder (Map.remove map name))) zipper |> moves ~id back, path
    | Shard _ -> failwith "rm: %s is a shard. Shards can't be removed." name
    | _ -> failwith "rm: %s is below a non-directory?" name
  )
  |> OTM.write
    
let mv ~id pathTo pathFrom monad =
  try 
    cp ~id pathTo pathFrom monad |> rm ~id pathFrom
  with PassControl _ ->
    Async.print_endline "Got to rm attempt";
    rm ~id pathFrom monad      

(** Command processing *)

let handle_mknode ~id message =
  match String.lsplit2 ~on:' ' message with
  | None -> failwith "Malformed input to mknode: %s" message
  | Some (path,rest) ->
     match String.lsplit2 ~on:' ' rest with
     | None -> failwith "Malformed input to mknode: %s" message
     | Some (nodeName,node) ->
        if nodeName = "."
        then mknode ~id (unmarshal_node node) path
        else mknode ~id ~nodeName (unmarshal_node node) path

let response_commands = ["ls";"help";"refresh";"commit"]
                    
let rec fscommands ~writer ~id =
  let move = move ~id ~command:(fun () -> "cd .") in
  let apply_path1 = apply_path1 ~id in
  let apply_path2 = apply_path2 ~id in
  let ls = ls ~writer in
  let help = help ~writer ~id in
  let commit =
    OTM.commit
      ~writer
      ~id
      ~run:perform_command
      ~update:(fun zipper -> globalZipper := go_to_root ~id zipper)
      globalZipper
  in
  [
    "cd", apply_path1 ~strict:true cd;
    "ls", apply_path1 ~strict:false ls;
    "cat", apply_path1 ~strict:false ls;
    "echo", echo ~id;
    "touch", apply_path1 ~strict:true touch;
    "mkdir", apply_path1 ~strict:true mkdir;
    "cp", apply_path2 cp;
    "rm", apply_path1 ~strict:false rm;
    "mv", apply_path2 mv;
    "prev", (fun _ -> OTM.map ~f:(move (Next false)));
    "next", (fun _ -> OTM.map ~f:(move (Next true)));
    "up", (fun _ -> OTM.map ~f:(move Up));
    "down", (fun _ -> OTM.map ~f:(move Down));
    "help", help;
    "quit", (fun _ m -> m);
    "commit", (fun _ -> commit);
    "refresh", (fun _ _ -> OTM.refresh !globalZipper);
    ]
      
and help ~writer ~id _ monad =
  Message("Commands: " ^ (String.concat ~sep:", " (List.map ~f:fst (fscommands ~writer ~id))) ^ "\n")
  |> write_response id writer;
  monad

and perform_command ~writer ~id input =
  (* TODO (jdl): This is an ugly hack to make prompts work. Think of a way to do better *)
  let wrap command ~f monad =
    if List.mem ~equal:String.equal response_commands command
    then f monad
    else
      let monad = f monad in
      write_response id writer Success;
      monad
  in
  let next b from =
    Hashtbl.set idToHost ~key:id ~data:Master;
    wrap "next" ~f:(OTM.map ~f:(move_to_next ~id from b))
  in
  match String.lsplit2 ~on:' ' input with
  | Some("mknode",rest) ->
     wrap "mknode" ~f:(handle_mknode ~id rest)
  | Some("next",from) when from <> "" -> next true from
  | Some("prev",from) when from <> "" -> next false from
  | _ -> 
     match String.split ~on:' ' input with
     | [] -> failwith "Got empty input: %s" input
     | command :: rest ->
        match List.Assoc.find ~equal:String.equal (fscommands ~writer ~id) command with
        | None -> fun _ -> failwith "Command `%s` does not exist" command
        | Some(f) -> wrap command ~f:(f rest)


let get_ith_folder folder i =
  Map.nth (Map.filter folder ~f:(function | Folder _ -> true | _ -> false)) (i-1)
        
let split_fs shard = function
  | Folder map ->
     begin
       match get_ith_folder map shard with
       | None -> failwith "Splitting failed: There are fewer than %i folders in root" shard
       | Some (name,folder) -> (name, folder)
     end     
  | _ -> failwith "Splitting failed: Root of filesystem was not a folder"

let get_master = function
  | Folder map -> Folder (Map.filter ~f:(function | File _ -> true | _ -> false) map)
  | _ -> failwith "Splitting failed: Root of filesystem was not a folder"

(** Async helpers *)
     
open Async
         
let check_id_location id =
  match Hashtbl.find idToHost id with
  | Some Master -> true
  | _ -> false

let print_prompt ~id writer zipper =
  if check_id_location id
  then
    let prompt = (make_path zipper ">") ^ " " in
    write_response id writer (Message(prompt))
  else ()
           
(** Client and shard handlers *)

let run_commands ~writer ~id ~afterF command (zipper,logs) =
  begin
    try
      let zipper,logs = 
        perform_command ~writer ~id command (OTM.return zipper)
        |> OTM.add_command (path_from_zipper zipper) command
        |> (Fn.flip OTM.runState) logs
      in
      (zipper,logs), true
    with
    | Failure(s) ->
       write_response id writer (Error(s));
       (zipper,logs), true
    | PassControl Master ->
       (zipper,logs), false
    | PassControl (Slave writer) ->
       Hashtbl.set idToHost ~key:id ~data:(Slave writer);
       (zipper,logs), false
  end |> afterF

(* TODO (jdl): Need to know when to respond with prompt from shard and when not too *)

let add_shard ~name ~shard () =
  match !globalZipper.current.term with
  | Folder map ->
     if Map.mem map name
     then
       failwith "Adding shard failed: A shard or folder with that name already exists"
     (* TODO (jdl): This will break if a user creates a folder of that name and then commits *)
     else
       let map = Map.set ~key:name ~data:shard map in
       globalZipper := {!globalZipper with current = {!globalZipper.current with term = Folder map}}
  | _ -> failwith "Adding shard failed: Root of filesystem was not a folder"

           
let deal_with_broken_shards () =
  let writerEq w1 w2 = Writer.id w1 = Writer.id w2 in
  Hash_set.filter_inplace
    shardSet
    ~f:(fun writer ->
      if Writer.is_closed writer
      then
        let _ =
          Hashtbl.mapi_inplace idToHost
            ~f:(fun ~key ~data -> match data with
              | Slave shardWriter when writerEq writer shardWriter ->
                 begin
                   match Hashtbl.find idToWriter key with
                   | None -> failwith "Id %s does not exist" (Writer.Id.to_string key)
                   | Some (clientWriter) -> 
                      let shardId = Async.Writer.id shardWriter |> Async.Writer.Id.to_string in
                      let string =
                        Printf.sprintf
                          "\nShard %s went down, returning to Master.\nPlease refresh.\n"
                          shardId
                      in
                      Writer.write clientWriter string
                 end;
                 Master
              | x -> x
            )
        in
        let _ =
          Mutex.critical_section commitMutex
            ~f:(fun () ->
              match !globalZipper.current.term with
              | Folder map ->
                 let map = 
                   Folder.filter map ~f:(function
                       | Shard shardWriter when writerEq writer shardWriter -> false
                       | _ -> true
                     )
                 in
                 globalZipper :=
                   {!globalZipper with current =
                                         {!globalZipper.current with term = Folder map}
                   }
              | _ -> failwith "Removing shard failed: Root of filesystem was not a folder"
            )
        in
        false
      else true
    )
           
let rec client_handler reader writer () =
  let client_handler = client_handler reader writer in
  let debug ?id = debug_message "M" in
  let id = Writer.id writer in
  read_marshal_and_run ~debug reader
    ~responsef:(fun _ -> Fn.compose return ignore)
    ~commandf:(fun _ input ->
      let afterF ((zipper,logs),prompt) =
        Hashtbl.set idToZiplogs ~key:id ~data:(zipper,logs);
        if prompt then print_prompt ~id writer zipper;
        client_handler ()
      in
      let (zipper,logs) =
        Option.value_exn
          ~message:("Id " ^ (Writer.Id.to_string id) ^ " does not exist")
          (Hashtbl.find idToZiplogs id)
      in
      deal_with_broken_shards ();
      match String.split ~on:' ' input with
      | "quit" :: _ ->
         Hash_set.iter shardSet ~f:(fun writer -> write_command id writer "quit");
         Hashtbl.remove idToZiplogs id;
         Hashtbl.remove idToWriter id;
         Hashtbl.remove idToHost id;
         return ()
      | "refresh" :: _ ->
         Hashtbl.set idToHost ~key:id ~data:Master;
         Hash_set.iter shardSet ~f:(fun writer -> write_command id writer "refresh");
         run_commands ~writer ~id ~afterF input (zipper,logs)
      | "commit" :: _ ->
         Hash_set.iter shardSet ~f:(fun writer -> write_command id writer "commit");
         run_commands ~writer ~id ~afterF input (zipper,logs)
      | _ ->
         match Hashtbl.find idToHost id with
         | None -> failwith "Id %s does not exist" (Writer.Id.to_string id)
         | Some (Slave writer)->
            write_command id writer input;
            client_handler ()
         | Some (Master)->
            run_commands ~writer ~id ~afterF input (zipper,logs)
    )

 
let rec shard_handler ~shard reader writer map =
  let shard_handler = shard_handler ~shard reader writer in
  let debug ?id = debug_message ?id ("S" ^ (string_of_int shard)) in
  let commandf id command =
    let zipper,logs = Option.value (Map.find map id) ~default:(!globalZipper,(OTM.create ())) in
    match String.split ~on:' ' command with
    | "quit" :: _ -> shard_handler (Map.remove map id)
    | _ -> 
       let afterF ((zipper,logs),prompt) =
         if prompt then print_prompt ~id writer zipper;
         shard_handler (Map.set map ~key:id ~data:(zipper,logs))
       in
       run_commands ~writer ~id ~afterF command (zipper,logs)
  in
  let responsef id response =
    failwith "Shard got response which should never happen"
  in
  read_marshal_and_run ~debug reader ~commandf ~responsef

let rec master_shard_handler reader writer () =
  let master_shard_handler = master_shard_handler reader writer in
  let debug ?id = debug_message ?id "M" in
  let commandf id command =
    match Hashtbl.find idToZiplogs id, Hashtbl.find idToWriter id with
    | None, _
    | _, None -> failwith "Id %s does not exist" (Writer.Id.to_string id)
    | Some (zipper,logs), Some writer ->
       let afterF ((zipper,logs),prompt) =
         Hashtbl.set idToZiplogs ~key:id ~data:(zipper,logs);
         if prompt then print_prompt ~id writer zipper;
         master_shard_handler ()
       in
       run_commands ~writer ~id ~afterF command (zipper,logs)
  in
  let responsef id response =
    match Hashtbl.find idToZiplogs id, Hashtbl.find idToWriter id with
    | None, _
    | _, None -> failwith "Id %s does not exist" (Writer.Id.to_string id)
    | Some (zipper,_), Some writer ->
       write_response id writer response;
       print_prompt ~id writer zipper;
       master_shard_handler ()
  in
  read_marshal_and_run ~debug reader ~commandf ~responsef

    
(** Setup functions for starting shards and master *)
    
(** Adds a newly connected shard to the globalZipper *)
let add_shard ~name ~shard () =
  match !globalZipper.current.term with
  | Folder map ->
     if Map.mem map name
     then
       failwith "Adding shard failed: A shard or folder with that name already exists"
     (* TODO (jdl): This will break if a user creates a folder of that name and then commits *)
     else
       let map = Map.set ~key:name ~data:shard map in
       globalZipper := {!globalZipper with current = {!globalZipper.current with term = Folder map}}
  | _ -> failwith "Adding shard failed: Root of filesystem was not a folder"

          
let start_master ~port fs =
  let current = {name = "/"; term = fs; updated = false} in  
  globalZipper := {current; parents = []};
  Tcp.Server.create
    ~on_handler_error:`Raise
    (Tcp.Where_to_listen.of_port port)
    (fun _ reader writer ->
      Writer.write_line writer "Please press enter to continue.";
      let debug = debug_message "M" in
      read_and_run ~debug reader
        ~f:(fun input ->
          match String.split ~on:' ' input with
          | "server" :: name :: [] ->
             let shard = Shard writer in
             Mutex.critical_section commitMutex ~f:(add_shard ~name ~shard);
             Hash_set.add shardSet writer;
             master_shard_handler reader writer ()
          | _ ->
             let zipper = !globalZipper in
             let id = Writer.id writer in
             Hashtbl.set idToZiplogs ~key:id ~data:(zipper,OTM.create ());
             Hashtbl.set idToWriter ~key:id ~data:writer;
             Hashtbl.set idToHost ~key:id ~data:Master;
             print_prompt ~id writer zipper;
             client_handler reader writer ()
        )
    )
  >>= fun _ -> Deferred.never ()

    
let start_shard ~port ~host ~shard fs =
  let name,fs = split_fs shard fs in
  Tcp.with_connection
    (Core.Host_and_port.create ~host ~port |> Tcp.Where_to_connect.of_host_and_port)
    (fun _ reader writer ->
      let current = {name; term = fs; updated = false} in
      let root = {name = "/"; term = Shard writer; updated = false} in
      globalZipper := {current; parents = [root]};
      let debug ?id = debug_message ?id ("S" ^ (string_of_int shard)) in
      read_and_run ~debug reader
        ~f:(fun _ ->
          Writer.write_line writer ("server " ^ name);
          shard_handler ~shard reader writer Writer.Id.Map.empty
        )
    ) >>= Deferred.never
    
let setup ~shard ~port ~host fs =
  match shard with
  | Some shard ->
     if shard = 0
     then start_master ~port (get_master fs)
     else start_shard ~port ~host ~shard fs 
  | None -> start_master ~port fs
       
(** The test filesystem and run command *)
let () =
  Command.async_spec
    ~summary:"Start file system server"
    Command.Spec.(
      empty
      +> flag "-shard" (optional int) (* 0 is master, 1 is first folder etc. *)
        ~doc:"Which shard to run, if any"
      +> flag "-port" (optional_with_default 8765 int)
        ~doc:"Port to listen on or connect to (if shard) (default 8765)"
      +> flag "-host" (optional_with_default "localhost" string)
        ~doc:"Host for shard to connect to (default 'localhost')"
    )
    (fun shard port host () -> setup ~shard ~port ~host ZfsUtils.simpleFS)
  |> Command.run
 *)
