(* TODOS:
 * - Figure out a good type for contract/op/check
 * - Change comprehension rep to list of strings and allow goto_child
 * -- Need to zip the names back in when going 'up'
 * - Add the fetch_s operations (for each primitive spec)
 * - Implement everything!
 *)

open Core
open Zfs
open ZFSAPI
open Result
open Result.Let_syntax

(* Types *)
type fs = ZFSAPI.t

type name = string [@@deriving show]
type path = string [@@deriving show]

module Var = String
type var = Var.t [@@deriving show]

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


type contract = (string * (fs -> ZFSAPI.t_or_fail)) list  [@@deriving show]

type fetch_md = path [@@deriving show]

type fetch_info =
  | FileRep of string
  | PathRep
  | PairRep
  | CompRep of (string list) [@@deriving show]

type fetch_result = fetch_md * fetch_info [@@deriving show]

type fNode =
  | NFile of string
  | NPath of string * node
  | NPair of node * node
  | NList of (name * node) list [@@deriving show]

and node =
  | Cursor of (fs -> node)
  | Forced of {contract : contract; node : fNode}  [@@deriving show]

type zipper =
  { ancestor : zipper option;
    left : node list;
    current : node;
    right: node list;
  } [@@deriving show]

type t = fs * zipper

type t_or_fail = (t,string) Core.result

let dummy_node   = Forced ({contract = []; node = NFile ""})
let dummy_zipper = {ancestor = None; left = []; current = dummy_node; right = []}


(* Helper functions *)
let failwith format = Printf.ksprintf failwith format

let debug = ref false
let set_debug () = debug := true

let p = Printf.printf

let d format =
  if !debug
  then Printf.ksprintf (Core.Printf.printf "Debug: %s\n%!") format
  else Printf.ksprintf Core.ignore format

let print_fetch_result = Fn.compose (Printf.printf "%s \n") show_fetch_result

let mk_err = Result.failf
(*format = Printf.ksprintf Result.fail format*)

let mk_ok t = Ok t

let extract_file fs =
  match fetch fs with
  | ZFSFile s -> NFile s
  | _ -> failwith "extract_file: Expected current node to be a file"

let extract_dir fs =
  match fetch fs with
  | ZFSDir l ->begin
    List.iter l ~f:(fun x -> d "element: %s" x);
    l
  end
  | _ -> failwith "extract_dir: Expected current node to be a directory"

let eval env = function
  | Var x ->
    begin
      match Map.find env x with
      | None -> failwith "eval: Variable %s not in environment" x
      | Some exp -> exp
    end
  | VDirList (x, f) -> begin
      match Map.find env x with
      | None -> failwith "eval: Variable %s not in environment" x
      | Some (Data s) -> begin
        DirList (f s)
      end
      | _ -> failwith "eval: Variable %s not bound to data in the env" x
  end
  | exp -> exp

let set_var env var exp = Map.set env ~key:var ~data:(eval env exp)

let str_exp env exp =
  match eval env exp with
  | Name s -> s
  | Data s -> s
  | _ -> failwith "str_exp: Expression did not evaluate to a string"

let list_exp env exp fs =
  match eval env exp with
  | List l -> l
  | DirList f -> List.filter ~f (extract_dir fs) |> List.map ~f:(fun s -> Name s)
  | _ -> failwith "list_exp: Expression did not evaluate to a list"

let id = Result.return

let force fs = function
  | Cursor f -> f fs
  | n -> n

let convert_result =
  map_error ~f:(function
  | IllegalAction s -> s
  | CommitFail -> "Failed to commit")

let get_from_contract s c =
  match List.Assoc.find c ~equal:String.equal s with
  | Some f -> fun t -> f t |> convert_result
  | None -> fun _ -> mk_err "Illegal move: %s" s

let get_contract z =
  match z.current with
  | Forced {contract; _} -> contract
  | _ -> failwith "get_contract: Node not forced"
(*
val node : specification -> path -> contract -> fs -> unit -> node
val mk_zipper : specification -> path -> zipper
*)

let get_name env e : string =
  match eval env e with
  | Name s -> s
  | _ -> failwith "name not avialable"


let rec n_op (op :t -> t_or_fail) (n :int) (t:t) : t_or_fail = begin
  if n = 0 then mk_ok t
  else op t >>= n_op op (n-1)
end

let rec count_shortest_prefix s1 s2 =
  match s1 , s2 with
  | h1::t1, h2::t2 ->begin
    if h1 = h2 then 1 + (count_shortest_prefix t1 t2) else 0
  end
  | _ -> 0

let add_node_type_name s = function
  | {current = Forced {node = NFile _; _}; _} -> s ^ "_file"
  | {current = Forced {node = NPath _; _}; _} -> s ^ "_path"
  | {current = Forced {node = NPair _; _}; _} -> s ^ "_pair"
  | {current = Forced {node = NList _; _}; _} -> s ^ "_list"
  | _ -> failwith "%s: Encountered unforced expression" s

let get_forest_path (fs, _) =
    ZFSAPI.get_path fs

let get_zfs_path (fs,_) =
    ZFSAPI.get_global_path fs


let sync_path ((fs,z) : t) : t_or_fail =
  let new_path = get_forest_path (fs, z) in
  let old_path = get_zfs_path (fs, z) in
  let _ = d "syncing path -- old path: %s new path: %s \n" old_path new_path in
    if old_path = new_path then mk_ok (fs, z)
    else
      let old_list = String.split old_path ~on:'/' in
      let new_list = String.split new_path ~on:'/' in
      let len_prefix = count_shortest_prefix old_list new_list in

      let _, old_suffix = List.split_n old_list len_prefix in
      let _, new_suffix = List.split_n new_list len_prefix in

      let backup_old = List.map old_suffix ~f:(fun _ -> "..") in


      let path_update = backup_old @ new_suffix in

      let path_update = String.concat ~sep:"/" path_update in

      let%bind fs' = ((ZFSAPI.goto path_update fs) |> convert_result) in
         mk_ok (fs', z)




(* Key functions *)
(* TODO:
 * - Properly implement and do CheckFS (See your earlier forest-notes)
 *)

let up (t : t) : t_or_fail =
  let%bind (fs, z) = sync_path t in
  let contract = get_contract z in
  let%bind fs = (get_from_contract "up" contract) fs in
  match z.ancestor with
  | Some ({current = Forced {node; contract}; _} as z') ->
    let%bind node =
      match node with
      | NPath(s,_) -> mk_ok (NPath(s,z.current))
      | NPair(_,_) ->
        begin
        match z.left, z.right with
        | [],(right :: []) -> mk_ok (NPair(z.current,right))
        | left :: [],[] -> mk_ok (NPair(left,z.current))
        | _ -> mk_err "up: Pairs broke"
        end
      | NList lst -> begin
        (*get the names, add them back to the nodes*)
        let names = List.map lst ~f:(fun (a,_) -> a) in
        let named = List.map2_exn names ((List.rev z.left) @ (z.current :: z.right)) ~f:(fun a b -> (a, b)) in
          mk_ok (NList named)
      end
      | _ -> mk_err "up: Something impossible happened. Debug"
    in
    mk_ok (fs, {z' with current = Forced {contract; node}})
  | _ -> mk_err "up: Node did not have an ancestor"

let down (t : t) : t_or_fail =
  let%bind (fs, z) = sync_path t in
  let%bind right, newCur, contract =
    match z.current with
    | Forced {node = NPath (_,n); contract}      -> ([]  ,n ,contract) |> mk_ok
    | Forced {node = NPair (n1,n2); contract}    -> ([n2],n1,contract) |> mk_ok
    | Forced {node = NList (hd::tl); contract} -> begin
      (*remove the names from the nodes*)
      let (_, hd), tl = hd , (List.map tl ~f:(fun (_,b) -> b)) in
        (tl  ,hd,contract) |> mk_ok
    end
    | _ -> mk_err "down: Node did not have any children"
  in
  let%map fs = (get_from_contract "down" contract) fs in
  fs, {ancestor = Some z; left = []; right; current = force fs newCur}

let next (t : t) : t_or_fail =
  let%bind (fs, z) = sync_path t in
  let%bind fs = (get_from_contract "next" (get_contract z)) fs in
  match z.right with
  | current :: right ->
    let z' =
      {z with left = z.current :: z.left;
              current = force fs current;
              right}
    in
    mk_ok (fs, z')
  | _ -> mk_err "next: Node did not have any right-siblings"


let prev (t : t) : t_or_fail =
  let%bind (fs, z) = sync_path t in
  let%bind fs = (get_from_contract "prev" (get_contract z)) fs in
  match z.left with
  | current :: left ->
    let z' =
      {z with left;
              current = force fs current;
              right = z.current :: z.right}
    in
    mk_ok (fs, z')
  | _ -> mk_err "prev: Node did not have any left-siblings"


let goto (name: string) (t : t) : t_or_fail =
  let%bind (fs, z) = sync_path t in
  match z.current with
  | Forced {node = NList (hd::tl);_} -> begin
    match (List.findi (hd::tl) ~f:(fun _ (n, _) -> n = name)) with
    | Some (i, _) -> down (fs, z) >>= (n_op next i)
    | None -> mk_err "goto: name not in comprehension"
  end
  | _ -> mk_err "goto: Node not a comprehension"





let rec fetch (t : t) :fetch_result =
  let (fs, z) = ok_or_failwith (sync_path t) in
  let fs = (get_from_contract (add_node_type_name "fetch" z) (get_contract z)) fs in
  let fs = ok_or_failwith fs in
  let fs_path : fetch_md = ZFSAPI.get_path fs in
  match z with
  | {current = Forced {node = NFile s; _}; _} -> (fs_path, FileRep s)
  | {current = Forced {node = NPath _; _}; _} -> begin
    match down (fs, z) with
    | Ok (fs', z') -> begin
    let result = fetch (fs', z') in
    let _ = up (fs', z') in
      result
    end
    | Error _ -> failwith "fetch: path does not have anything below it"
  end
  | {current = Forced {node = NPair _; _}; _} -> (fs_path, PairRep)
  | {current = Forced {node = NList l; _}; _} -> (fs_path, CompRep (List.map l ~f:(fun (name, _) -> name)) )
  | _ -> failwith "fetch: Encountered unforced expression"


let shallow_fetch (t : t) :fetch_result =
  let (fs, z) = ok_or_failwith (sync_path t) in
  let _ = ZFSAPI.fetch fs in
  let fs_path : fetch_md = ZFSAPI.get_path fs in
  match z with
  | {current = Forced {node = NFile s; _}; _} -> (fs_path, FileRep s)
  | {current = Forced {node = NPath _; _}; _} -> (fs_path, PathRep)
  | {current = Forced {node = NPair _; _}; _} -> (fs_path, PairRep)
  | {current = Forced {node = NList l; _}; _} -> (fs_path, CompRep (List.map l ~f:(fun (name, _) -> name)) )
  | _ -> failwith "fetch: Encountered unforced expression"

let print = Fn.compose print_fetch_result fetch
let print_with_newline t = print t; print_endline ""

let debug_print (_,z) = Printf.printf "%s\n" (show_zipper z)




let rec node s c env fs =
  let ignore_fetch fs = Core.ignore (ZFSAPI.fetch fs); mk_ok fs in
  match s with
  | File ->
    (* TODO: Fix contract, make sure p not necessary *)
    Forced { contract = c @ [("fetch_file",ignore_fetch);("put_file",id)];
             node = extract_file fs}

  | PathExp (e,s) ->
    let name = str_exp env e in
    if ZFSAPI.has_child name fs
    then
      let _ = d "FS has child %s" name in
      let cursor = node s ["up",ZFSAPI.up] env in
      let c' = ["down",ZFSAPI.goto_child name] in
      Forced { contract = c @ c' @ [("fetch_path",ignore_fetch);("put_path",id)] ;
               node = NPath(name, Cursor cursor) }
    else
      let _ = d "FS does not have child %s" name in
      let cursor = node s ["up",ZFSAPI.up] env in
      let c' = [] in
      Forced { contract = c @ c';
               node = NPath(name, Cursor cursor) }

  | DPair (x, s1, s2) ->
      let curL = node s1 [("up",id);("next",id)] env in
      let curR = (fun fs ->
        let z = {dummy_zipper with current = (curL fs)} in
        let env' =
          match fetch (fs, z) with
          | _, FileRep s ->
             d ">>> Setting %s to %s" x s;
             set_var env x (Data s)
          | _, CompRep l -> begin
            (*is this what we want or do we want something like below*)
            set_var env x ( List (List.map l ~f:(fun s -> Data s)))
          end
          | _ -> begin
            d "left of dpair is not a file";
            set_var env x EmptyExp
          end
        in
          node s2 [("up",id);("prev",id)] env' fs
      ) in

      let c' = ["down",id] in
        Forced { contract = c @ c' @ [("fetch_pair",ignore_fetch);("put_pair",id)];
                 node = NPair(Cursor curL,Cursor curR)}

  | IPair (s1,s2) ->
    let curL = node s1 [("up",id);("next",id)] env in
    let curR = node s2 [("up",id);("prev",id)] env in
    let c' = ["down",id] in
    Forced { contract = c @ c' @ [("fetch_pair",ignore_fetch);("put_pair",id)];
             node = NPair(Cursor curL,Cursor curR)}

  | Comp (s,x,e) ->
    let ci = [("up",id);("next",id);("prev",id)] in
    let lst,c' =
      match List.rev (list_exp env e fs) with
      | [] -> [],[]
      | e :: [] -> [(get_name env e,  Cursor (node s ["up",id] (set_var env x e)))],["down",id]
      | en :: tl ->
        begin
          match List.rev tl with
          (*here in map evaluate to a string add this to the rep for todo*)
          | e1 :: rest ->
            (get_name env e1, Cursor (node s [("up",id);("next",id)] (set_var env x e1)))
            ::
            (List.map ~f:(fun e -> (get_name env e, Cursor (node s ci (set_var env x e)))) rest)
            @
            [(get_name env en, Cursor (node s [("up",id);("prev",id)] (set_var env x en)))]
            ,["down",id]
          | _ -> failwith "node: This is impossible"
        end
    in
    Forced {contract = c @ c' @ [("fetch_list",ignore_fetch);("put_list",id)];
            node = NList lst}

(* User exposed functions *)
(* TODO: Make sure you should goto p and not 'start at p' *)
let mk_zipper s p =
  let%map fs = ZFSAPI.create () |> ZFSAPI.goto p |> convert_result in
  (fs,{dummy_zipper with current = node s [] String.Map.empty fs})

let put n (t : t) : t_or_fail =
  let%bind (fs, z) = sync_path t in
  match n,z with
  | FileRep s, {current = Forced {node = NFile _; contract}; _} ->
    let%map fs = update s fs |> convert_result in
    (fs,{z with current = Forced {node = NFile s; contract}})

  | PathRep, {current = Forced {node = NPath _; _}; _} -> mk_ok (fs,z)
  | PairRep, {current = Forced {node = NPair _; _}; _} -> mk_ok (fs,z)
  | CompRep _, {current = Forced {node = NList _; _}; _} -> mk_ok (fs,z)
  | _ -> mk_err "put: Incompatible node types"

let finish (fs,_) = destroy fs

let run_txn ~f s p () =
  let f_to_z fs =
    match ZFSAPI.goto p fs with
    | Error _ -> failwith "run_txn: path %s does not exist" p
    | Ok fs ->
       (fs,{dummy_zipper with current = node s [] String.Map.empty fs})
       |> f
       |> fst
  in
  run_txn ~f:f_to_z ()

let run_txn_exn ~f = run_txn ~f:(Fn.compose Core.Result.ok_or_failwith f)


let check name (_,z) =
  match z with
  | {current = Forced {contract; _}; _} ->
    List.Assoc.mem ~equal:String.equal contract name
  | _ -> failwith "check: Encountered unenforced expression"


(*[map_reduce_children map_reduce init t] applies map_reduce to all the children
 * of t using init in the order they are in the zipper*)
let map_reduce_children (map_reduce: 'a -> t -> ('a * t_or_fail,string) Core.result) (init: 'a) (t : t) :('a * t_or_fail,string) Core.result =
    let _, z = t in
    match z with
    | {current = Forced {node = NFile s; _}; _} -> begin
      d "map_reduce_postorder: File node containing: %s\n" s;
      mk_ok (init, mk_ok t)
    end
    | {current = Forced {node = NPath _; _}; _} -> begin
      d "map_reduce_postorder: Path node\n";
      let%map (init', t') = down t >>= map_reduce init in
      let t'' = t' >>= up in
        (init', t'')
    end
    | {current = Forced {node = NPair _; _}; _} -> begin
      d "map_reduce_postorder: Pair node\n";
      let%bind init', t' = down t >>= map_reduce init  in
      let%map init'', t'' = t' >>= next >>= map_reduce init' in
      let t''' = t'' >>= up in
        init'', t'''
    end
    | {current = Forced {node = NList l; _}; _} -> begin
      d "map_reduce_postorder: List node\n";
      let rec mrn (n :int) (init: 'a) (t:t) : ('a * t_or_fail, string) Core.result = begin
        if n = 1 then map_reduce init t
        else
          let%bind init', t' = map_reduce init t in
          let%bind t'' = t' >>= next in
            mrn (n-1) init'  t''
      end in
      let%map init', t' = down t >>= mrn (List.length l) init in
      let t'' = t' >>= up in
        (init', t'')
    end
    | _ -> failwith "map_reduce: Encountered unforced expression"


let map_reduce_postorder ~(m: t -> t_or_fail) ~(f: 'a -> t -> 'a) ~(init : 'a) (t : t) : 'a * t_or_fail =
  let rec map_reduce (init : 'a) (t : t) : ('a * t_or_fail,string) Core.result  =
    (*apply to children*)
    let%bind (init', t') = map_reduce_children map_reduce init t in
    (*apply to node*)
    let%map t'' = t' >>= m in
    let init'' = f init' t'' in
      (init'', mk_ok t'')
  in
  match map_reduce init t with
  | Ok (a,b) -> (a,b)
  | _ -> failwith "problem encountered in map reduce"


let map_postorder ~(m: t -> t_or_fail) (t : t) : t_or_fail =
  let _, result = map_reduce_postorder ~m:m ~f:(fun () _ -> ()) ~init:() t in
    result

let fold_postorder ~(f: 'a -> t -> 'a)  (init : 'a) (t : t) : 'a =
  let result, _ = map_reduce_postorder ~m:(fun z -> mk_ok z) ~f:f ~init:init t in
    result

let map_reduce_preorder ~(m: t -> t_or_fail) ~(f: 'a -> t -> 'a) ~(init : 'a) (t : t) : 'a * t_or_fail =
  let rec map_reduce (init : 'a) (t : t) : ('a * t_or_fail,string) Core.result  =
    (*apply m and f to node*)
    let%bind t' = m t in
    let init' = f init t' in
      (*apply to children*)
      map_reduce_children map_reduce init' t'
  in
  match map_reduce init t with
  | Ok (a,b) -> (a,b)
  | _ -> failwith "problem encountered in map reduce"

let map_preorder ~(m: t -> t_or_fail) (t : t) : t_or_fail =
  let _, result = map_reduce_preorder ~m:m ~f:(fun () _ -> ()) ~init:() t in
    result

let fold_preorder ~(f: 'a -> t -> 'a)  (init : 'a) (t : t) : 'a =
  let result, _ = map_reduce_preorder ~m:(fun z -> mk_ok z) ~f:f ~init:init t in
    result