open Core
open Types
open ZFSUtils

(* TODOS:
 * - BEFORE UPDATING TO PROPER FS, STARE HARD AT FS UPDATING PARTS OF TxZFS
 * - Make normal ZFS properly update the FS (and not just globalZipper)
 * - Decide whether to allow other inputs to AddChild 
 *   than empty folders or files.
 * -- Then update it
 * - For TxZFS, maybe create some way to compute a path delta 
 *   to make global zipper updating more efficent
 * - Try to populate global zipper as local operations happen
 *   to improve sharing
 * -- This is only important when connected to real FS
 * - See inline TODOs
 *)

module type ZFS = sig
  type context

  type context_or_fail = (context,fsErr) Core.result

  val step : context -> context_or_fail

  val apply_command : context -> context_or_fail Async.Deferred.t

  val init_context : unit -> context

  val get_zipper : context -> fsZipper

  val add_command : fsCommand -> context -> context

  val remove_command : context -> context
end

let initial_zipper = 
  { ancestor = None; left = []; current = ("/",Unforced); right = []}

module ZFS  = struct

  type context = fsZipper * fsCommand

  type context_or_fail = (context,fsErr) Core.result

  let globalZipper = ref initial_zipper  
  
  let smash_command c (z,_) = (z,c)

  let rec step (z,command) =
    match command with
    | Up   -> 
      let opt =
        let open Option.Let_syntax in
        let%map ancestor = z.ancestor in
        let t = Dir(List.rev z.left @ (z.current :: z.right)) in 
        let z' = {ancestor with current = (get_name ancestor,t)} in
        mk_ok (z',Skip)
      in
      Option.value opt
      ~default:(mk_err "Up: Tried to go up with no ancestor")

    | Down -> 
      begin
        match fetch_unforced z with
        | FDir (name :: _) -> mk_ok (z,GotoChild name)
        | _ -> mk_err "Down: No first child"
      end

    | Next ->
      begin
        match z.right with
        | current :: right ->
          let z' = {z with left = z.current :: z.left; current; right;} in
          mk_ok (z',Skip)
        | _ -> mk_err "Next: No next sibling"
      end

    | Prev -> 
      begin
        match z.left with
        | current :: left ->
          let z' = {z with left; current; right = z.current :: z.right;} in
          mk_ok (z',Skip)
        | _ -> mk_err "Prev: No previous sibling"
      end

    | GotoChild s ->
      let z = force z in
      begin
      match z.current with
      | (name, Dir lst) -> 
        let (left,rest) =
          List.split_while ~f:(fun (name,_) -> not (String.equal s name)) lst
        in
        begin
          match rest with
          | current :: right ->
            let z' =
              { ancestor = Some({z with current = (name,Dir [])});
                left = List.rev left;
                current;
                right;
              }
            in
            mk_ok (z', Skip)
          | [] -> mk_err "GotoChild: %s is not in the directory" s
        end
      | _ -> mk_err "GotoChild: Current node is not a directory"
      end

    | Goto p -> mk_ok (z,path_eval z p |> flatten_seq)
    | GoDoReturn (p,c) ->
      let c' = Seq (path_eval z p, Seq (c,apath_eval z)) |> flatten_seq in
      mk_ok (z,c')


    | Update s -> 
      let z = force z in
      if is_dir z
      then mk_err "Update: Current node is not a file"
      else
        let z' = {z with current = (get_name z, File s)} in
        mk_ok (z',Skip)

    | RemoveChild s -> 
      let z = force z in
      begin
      match z.current with
      | (name, Dir lst) -> 
        let (left,rest) =
          List.split_while ~f:(fun (name,_) -> not (String.equal s name)) lst
        in
        begin
          match rest with
          | _ :: right ->  mk_ok ({ z with current = (name, Dir(left @ right))}, Skip)
          | [] -> mk_err "RemoveChild: %s is not in the directory" s
        end
      | _ -> mk_err "RemoveChild: Current node is not a directory"
      end 

    | AddChild (s,n) -> 
      let z = force z in
      if has_child s z
      then mk_err "AddChild: Directory already has a child named %s" s
      else
        begin
          match z.current with
          | (name, Dir lst) ->
            let lst' = 
              List.merge lst [(s,fs_of_res n)] 
              ~compare:(fun (s1,_) (s2,_) -> String.compare s1 s2)
            in
            mk_ok ({ z with current = (name, Dir lst')}, Skip)
          | _ -> mk_err "AddChild: Current node is not a directory"
        end

    | Seq (Skip,c2) -> mk_ok (z,c2)

      (* TODO: This means you can't sequence stuff after Commit... 
       * This is only to avoid weirdly nested sequences, so it could
       * be solved by forcing a flattening and removing this.
       *)
    | Seq (Commit,_) -> mk_ok (z, Commit)
    | Seq (c1,c2) -> 
      let open Result in
      step (z,c1) 
      >>| fun (z',c1') -> (z',Seq (c1',c2))

    | Inspect -> mk_ok (force z,Skip)
    | Commit -> mk_err "Can't step from Commit"

    | Skip  -> mk_err "Can't step from Skip"

  (* TODO: Consider flattening command pre-stepping*)
  let rec apply_command ((_,command) as context) = 
    let open Async in
    match command with
    | Skip -> mk_ok context |> return
    | Commit
    | Seq(Commit,_) -> 
      let root_context = smash_command (Goto Root) context |> apply_command in
      Async.Deferred.Result.map root_context 
        ~f:(fun (z,_) -> 
          globalZipper := z;
          (z,Skip)
        )
      | _ -> Async.Deferred.Result.bind (step context |> return) ~f:apply_command

  let init_context () = (!globalZipper,Skip)

  let add_command c (z,c') = (z,Seq(c',c))

  let remove_command = smash_command Skip

  let get_zipper (z,_) = z
end

module TxZFS : ZFS = struct
  type path_string = string 

  type log_entry = 
    | LRead of path_string
    | LWrite of data * path_string
    | LAddChild of fetchRes * path_string
    | LRemChild of path_string

  type local_log = log_entry list

  type timestamp = int
  type global_log = (timestamp * log_entry) list

  type thread = fsZipper * fsCommand
  type transaction_state = fsCommand * timestamp * local_log

  type context = thread * transaction_state
  

  type context_or_fail = (context,fsErr) Core.result

  (* Initialization stuff *)

  let globalZipper = ref initial_zipper
  let globalMutex = Mutex.create ()
  let gts = ref 0
  let globalLog : global_log ref = ref []

  let increment_and_get_ts () = incr gts; !gts

  let init_context () = 
    (!globalZipper, Skip),(Skip,increment_and_get_ts (), [])

  let add_command c2 ((z,c1),(cs,ts,ll)) = 
    ((z,Seq(c1,c2)),(Seq(cs,c2),ts,ll))

  let get_zipper ((z,_),_) = z
  let get_command ((_,c),_) = c
  let get_ts (_,(_,ts,_)) = ts
  
  let smash_command c ((z,_),ts) = ((z,c),ts)

  let remove_command = smash_command Skip

  let get_entries start = 
    List.filter_map 
      ~f:(fun (ts,le) -> if ts >= start then Some(le) else None)

  let extract_writes = List.filter ~f:(function | LRead _ -> false | _ -> true)


  (* Changes compared to formalization:
    * - Skipped logging and Next and Prev since same thing must already
    *   be in log for us to have gotten to that node
    * - Log entries don't contain the nodes read, since those are only
    *   used for proofs, and never for algorithmic purposes.
    *)
  let rec step (((z,command) as thread,(cs,ts,ll)) as context) =
    let step_n_add (thread,(cs,ts,ll)) le = 
      Result.map (ZFS.step thread)
      ~f:(fun thread -> (thread,(cs,ts,le :: ll)))
    in
    match command with
    | Update data -> 
      let le = LWrite(data, get_path_s z) in
      step_n_add context le

    | RemoveChild name -> 
      let le = LRemChild(Filename.concat (get_path_s z) (name)) in
      step_n_add context le

    | AddChild (name,node) -> 
      let path = Filename.concat (get_path_s z) (name) in
      let le = LAddChild(node, path) in
      step_n_add context le

    | Seq (Skip,c2) -> mk_ok ((z,c2),(cs,ts,ll))
    | Seq (Commit,_) -> mk_ok ((z, Commit),(cs,ts,ll))
    | Seq (c1,c2) -> 
      let open Result.Let_syntax in
      let%map context = smash_command c1 context |> step in
      smash_command (Seq (get_command context,c2)) context

    | Down 
    | GotoChild _
    | Inspect -> step_n_add context (LRead(get_path_s z))

    | _ -> Result.map (ZFS.step thread) ~f:(fun thread -> thread,(cs,ts,ll))

  and apply_command (((_,command),_) as context) = 
    let open Async in
    match command with
    | Skip -> mk_ok context |> return
    | Commit
    | Seq(Commit,_) -> commit context
    | _ -> Async.Deferred.Result.bind (step context |> return) ~f:apply_command

  and apply_command_nodef (((_,command),(_,_,_)) as context) = 
    match command with
    | Skip -> mk_ok context
    | _ -> Result.bind (step context) ~f:apply_command_nodef

  and commit (_,(_,ts,ll)) = 
    let check_log log ts = 
      let rec extract_reads = function
        | LRead(p) :: tl 
        | LWrite(_,p) :: tl -> p :: (extract_reads tl)
        | LAddChild(_,p) :: tl 
        | LRemChild(p) :: tl -> (Filename.dirname p) :: (extract_reads tl)
        | [] -> []
      in
      (* TODO: MAKE SURE YOUR PATHS ARE APPROPRIATELY CANONICAL! *)
      let conflict_path p' = function
        | LWrite(_,p) -> p = p'
        | LAddChild(_,p)  
        | LRemChild(p) -> String.is_prefix p' ~prefix:p 
        | _ -> failwith "conflict_path: Got a read"
      in
      let relevant_gl = get_entries ts !globalLog in
      List.for_all (extract_reads log) ~f:(fun p ->
        List.for_all relevant_gl ~f:(conflict_path p)
      )
    in

    let merge log = 
      let context = init_context () in
      let writes = extract_writes log in
      let log_to_com = function
        | LWrite(data,p) -> Seq(Goto (path_of_string p),Update data)
        | LAddChild(node,p) -> 
          let (dirname,basename) = Filename.split p in
          Seq(Goto (path_of_string dirname),AddChild (basename,node))
        | LRemChild(p) -> 
          let (dirname,basename) = Filename.split p in
          Seq(Goto (path_of_string dirname),RemoveChild basename)
        | _ -> failwith "merge: Got a read"
      in
      let open Result in
      let update context le = 
        context 
        >>| smash_command (log_to_com le)
        >>= apply_command_nodef
      in
      List.fold ~init:(Result.return context) ~f:update writes
      >>| smash_command (Goto Root)
      >>= apply_command_nodef
      >>| fun context ->
        globalZipper := get_zipper context;

        (* Updating underlying FS
        * TODO: it's not clear that this is quite safe though *)
        List.iter writes ~f:(function
          | LWrite(data,p) -> FS.update p (FFile data)
          | LAddChild(node,p) -> 
            let (dirname,basename) = Filename.split p in
            FS.add_child dirname basename node
          | LRemChild(p) -> 
            let (dirname,basename) = Filename.split p in
            FS.rem_child dirname basename
          | _ -> failwith "merge: Got a read"
        );

        (* Adds relevant entries to global log *)
        globalLog := List.append !globalLog (List.map ~f:(fun le -> (get_ts context,le)) writes);
        init_context ()
    in
    let open Result in
    let lrev = List.rev ll in
    let context =
      Mutex.critical_section globalMutex
      ~f:(fun () ->
        if check_log lrev ts
        then (d "Commit succeeded!"; merge lrev |> return)
        else (d "Commit failed!"; CommitFail |> fail)
        )
      >>| fun _ -> init_context ()
    in
    Async.return context
end