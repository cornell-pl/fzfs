open Core
open Async
open Types
open ZFSUtils

type t = Types.fsZipper * Reader.t * Writer.t

type node_type =
  | ZFSDir of string list
  | ZFSFile of string

type fsErr = Types.fsErr =
  | CommitFail
  | IllegalAction of string

type t_or_fail = (t,fsErr) Core.result

type path = string
type name = string
type data = string

(* Helper functions *)

let block = Thread_safe.block_on_async_exn

let send_and_receive : fsCommand -> t -> t_or_fail
  = fun command (_,reader,writer) ->
  block
  ( fun () ->
    write_struct writer command;
    Reader.read_marshal reader
    >>| function
    | `Eof -> failwith "send_and_receive: No response from ZFS"
    | `Ok (FSErr e) -> Result.fail e
    | `Ok (FSZip z) -> Result.return (z,reader,writer)
  )

(* Main functions *)
let create ?(port=8765) ?(host="localhost") () : t =
  block (
    fun () ->
    Tcp.connect
      (Core.Host_and_port.create ~host ~port
      |> Tcp.Where_to_connect.of_host_and_port)
    >>= fun (_,reader,writer)
    -> Reader.read_marshal reader
    >>| function
    | `Eof -> failwith "create: No response from ZFS"
    | `Ok (FSErr (IllegalAction s)) -> failwith "create: Returned an error: %s" s
    | `Ok (FSErr CommitFail) -> failwith "create: Returned a commit error"
    | `Ok (FSZip z) -> (z,reader,writer)
  )
(* let marshRead = read_marshal_and_run reader ~debug:(fun ?id _ -> ignore id) in
   marshRead ~f:(fun zipper -> shell_loop marshRead writer zipper)
*)

(* 'close' writer then reader *)
let destroy_no_commit (_,r,w) =
  (Writer.close w >>= fun () ->  Reader.close r)
  |> don't_wait_for

let destroy t =
  match send_and_receive Commit t with
  | Error (IllegalAction s) -> failwith "Tried to commit and got Error: %s" s
  | Error CommitFail -> destroy_no_commit t; false
  | Ok _ -> destroy_no_commit t; true

let rec run_txn ?(port=8765) ?(host="localhost") ~f () =
  let run_txn = run_txn ~port ~host ~f in
  create ~port ~host () |> f
  |> send_and_receive Commit
  |> function
    | Ok t -> destroy_no_commit t
    | Error (IllegalAction s) ->
      failwith "run_txn: Got IllegalAction error: %s" s
    | Error CommitFail -> run_txn ()

let up   = send_and_receive Up
let down = send_and_receive Down
let next = send_and_receive Next
let prev = send_and_receive Prev
let goto p = send_and_receive (Goto (path_of_string p))

let go_do_return ~f p ((z,_,_) as t) =
  let open Result.Let_syntax in
  let%bind t = send_and_receive (Goto (path_of_string p)) t in
  let%bind ret, t = f t in
  let%map t = send_and_receive (Goto (get_path_p z)) t in
  (ret,t)

let goto_child s = send_and_receive (GotoChild s)
let update s = send_and_receive (Update s)
let remove_child s = send_and_receive (RemoveChild s)
let mkdir s = send_and_receive (AddChild (s, FDir []))
let mkfile s = send_and_receive (AddChild (s, FFile ""))

let fetch t =
  let open Result in
  match send_and_receive Inspect t with
  | Error (IllegalAction s) -> failwith "fetch: Returned an error: %s" s
  | Error CommitFail -> failwith "fetch: Returned a commit error"
  | Ok (z,_,_) ->
    begin
      match fetch z with
      | FDir lst -> ZFSDir lst
      | FFile s -> ZFSFile s
    end

let has_parent (z,_,_) = Option.is_some z.ancestor

let is_dir = seq fetch (function | ZFSDir _ -> true | _ -> false)

let has_child s =  seq fetch
  (function | ZFSDir l when List.exists ~f:(String.equal s) l -> true | _ ->
  false)

let get_path (z,_,_) = get_path_s z

let get_global_path t =
  let open Result in
  match send_and_receive Inspect t with
  | Error (IllegalAction s) -> failwith "fetch: Returned an error: %s" s
  | Error CommitFail -> failwith "fetch: Returned a commit error"
  | Ok (z,_,_) -> get_path_s z

