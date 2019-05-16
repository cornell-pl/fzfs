(* TODOS:
 * - Implement cp and mv 
 *)

open Core
open Zfs.ZFSAPI

(* Helper functions *)



let norm_write s = 
  let open Out_channel in
  output_string stdout s;
  flush stdout

let memory_table = Int.Table.create ()
let write = ref norm_write

let make_prompt t = get_path t ^ "> "

let read_and_prompt n t = 
  match Int.Table.find memory_table n with
  | Some input -> input
  | None ->
    !write (make_prompt t);
    let input = 
      In_channel.input_line ~fix_win_eol:true In_channel.stdin
      |> Option.value_exn ~message:"read: Failed to receive input"
    in
    Int.Table.set ~key:n ~data:input memory_table;
    input

let write_endline s = !write (s ^ "\n")
let write_format format = Core.Printf.ksprintf write_endline format

let mk_error format = Core.Printf.ksprintf (fun s -> Error (IllegalAction s)) format

(* Main logic *)
                    
let rec fscommands =
  let open Result in
  let mal_exp = mk_error "Malformed expression" in
  let print_node = function
    | ZFSDir l -> List.iter ~f:write_endline l
    | ZFSFile s -> !write s
  in
  let arg0 ~f t = function
    | [] -> f t
    | _ -> mal_exp
  in
  let arg1 ~f t = function
    | hd :: [] -> f hd t
    | _ -> mal_exp
  in
  let argE ~f t = function
    | [] -> f t >>| snd
    | hd :: [] -> go_do_return ~f hd t >>| snd
    | _ -> mal_exp
  in
  let fetch = argE ~f:(fun t -> fetch t |> print_node; return ((), t)) in
  let update t rest = 
    (* Mostly error checking for strings *)
    let string = String.concat ~sep:" " rest in
    match String.index_from string 1 '"' with
    | Some(i) when String.length string - 1 = i ->
        if String.get string 0 = '"'
        then update (String.sub ~pos:1 ~len:(i-1) string) t
        else mk_error "Update: Expected string, got %s" string
    | _ -> mk_error "Update: Expected string, got %s" string
  in
  [
    "cd", arg1 ~f:goto;
    "ls", fetch;
    "cat", fetch;
    "fetch", fetch;
    "update", update;
    "prev", arg0 ~f:prev;
    "next", arg0 ~f:next;
    "up", arg0 ~f:up;
    "down", arg0 ~f:down;
    "touch", arg1 ~f:mkfile;
    "mkdir", arg1 ~f:mkdir;
    "rm", arg1 ~f:remove_child;
    "quit", arg0 ~f:return;
    "help", help;
    ]
    
and help t _ =
  "Commands: " ^ (String.concat ~sep:", " (List.map ~f:fst fscommands))
  |> write_endline;
  Result.return t


and perform_command input t =
  match String.split ~on:' ' input with
  | [] -> mk_error "Got empty input: %s" input
  | command :: rest ->
    match List.Assoc.find ~equal:String.equal fscommands command with
    | None -> mk_error "Command `%s` does not exist" command
    | Some(f) -> f t rest

let rec shell_loop n t =
  let input = read_and_prompt n t in
  if String.is_prefix ~prefix:"quit" input
  then (write := ignore; t)
  else
    match perform_command input t with
    | Error CommitFail -> failwith "shell_loop: Got a Commit Failure"
    | Error (IllegalAction s) -> 
      write_format "Error: %s" s; 
      shell_loop (n+1) t
    | Ok t -> shell_loop (n+1) t

let start_client ~port ~host () =
  run_txn ~port ~host ~f:(shell_loop 0) ()

let () =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Start filesystem client (MAKE SURE TO START SERVER FIRST!)"
    [%map_open
     let port = 
      flag "-port" (optional_with_default 8765 int) 
      ~doc:"Port to listen on or connect to (if shard) (default 8765)"
     and host =
       flag "-host" (optional_with_default "localhost" string)
       ~doc:"Host for shard to connect to (default 'localhost')"
     in
     fun () -> start_client ~port ~host ()
    ] |> Command.run