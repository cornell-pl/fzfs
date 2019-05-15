open Core
open Async
type path = string
type fileName = string
type fileContent = string

(* Async and messaging *)
                     
type fileType =
  | D
  | F
                     
type response_type =
  | Message of string 
  | Error of string
  | Dir of (fileName * fileType) list
  | File of fileContent
  | Success
              
type message =
  | Command of Writer.Id.t * string
  | Response of Writer.Id.t * response_type
              
let value_or_fail message = function
  | None -> failwith message
  | Some x -> x

let failwith format = Printf.ksprintf failwith format
                                      
let write_message = Writer.write_marshal ~flags:[Marshal.No_sharing]

let check_writer writer =
  if Writer.is_closed writer
  then
    let shardId = Writer.id writer |> Writer.Id.to_string in
    failwith "Shard %s is down. Please refresh." shardId
                                               
let write_command id writer command =
  check_writer writer;
  write_message writer (Command(id,command))
                
let write_response id writer response =
  check_writer writer;
  write_message writer (Response(id,response))

let debug_message ?id smsg input =
  let input = Core.String.rstrip ~drop:((=) '\n') input in
  match id with
  | None -> print_endline (Printf.sprintf "%s - %s" smsg input)
  | Some id -> print_endline (Printf.sprintf "%s - %s: %s" smsg (Writer.Id.to_string id) input)

let read_and_run_poly reader ~init ~debug ~f = 
  Reader.read_line reader
  >>= function
  | `Eof -> return init
  | `Ok input ->
     debug input;
     f input
                             
let read_marshal_and_run_poly
      reader
      ~init
      ~(debug:?id:Writer.Id.t -> string -> unit)
      ~commandf
      ~responsef = 
  Reader.read_marshal reader
  >>= function
  | `Eof -> return init
  | `Ok (Command (id,command)) -> 
     debug ~id command;
     commandf id command
  | `Ok (Response (id,response)) ->
     begin
       match response with
       | Message s -> debug ~id ("Message: " ^ s)
       | Error s -> debug ~id ("Error: " ^ s)
       | Dir _ -> debug ~id "Dir"
       | File _ -> debug ~id "File"
       | Success -> debug ~id "Success"
     end;
     responsef id response

let read_marshal_and_run = read_marshal_and_run_poly ~init:()
let read_and_run = read_and_run_poly ~init:()

module WriterSet =
  Core.Hash_set.Make(
      struct
        include Writer
        let hash writer = id writer |> Id.hash
        let compare writer1 writer2 = Id.compare (id writer1) (id writer2)
        let t_of_sexp sexp = failwith "Hopefully you don't actually need this"
      end
    )
