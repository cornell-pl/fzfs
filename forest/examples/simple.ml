open Core
open ExUtils
open Result
open Result.Let_syntax
open Forest


let always _ = true




[%%txforest {|

  d = directory {
    index is "index.txt" :: file;
    dir is "dir" :: [x :: file | x <- index ]
  }

  spec = d

|}]


let explore (zipper:t) : t_or_fail =
  down zipper >>= next >>= (fun z -> print z; mk_ok z)

let get_info_for_id (id: int) (zipper:t) : t_or_fail =
  down zipper >>= next >>= down >>= goto_pos_comp id >>= (fun z -> get_and_print z; mk_ok zipper)


let get_info_for_name (name:string) (zipper: t) : t_or_fail =
  down zipper >>= next >>= down >>= goto name >>= down >>= (fun z -> get_and_print z; mk_ok zipper)

let main (trans :int) (id: int option) (name:string option) (debug: bool) (zipper:t) : t =
  begin
    if debug then Forest.set_debug () else ();
    match trans with
    | 0 -> begin
      explore zipper
    end
    | 1 -> begin
      match id with
      | Some i -> get_info_for_id i zipper
      | _ -> failwith "Missing parameter for transaction: %d" trans
    end
    | 2 -> begin
      match name with
      | Some n -> get_info_for_name n zipper
      | _ -> failwith "Missing parameter for transaction: %d" trans
    end
    | _ -> failwith "Transaction %d is not implemented yet" trans
  end
  |> function
  | Ok zipper -> zipper
  | Error s -> p "Error: %s" s; zipper



let () =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Runs various transactions on the 'simple' filestore "
    [%map_open
      let trans = flag "trans" (required int) ~doc:"[0-2] Run this transaction"
      and id = flag "id" ( optional int )
                  ~doc:"index of the file"
      and name = flag "name" (optional string)
                  ~doc:"name of the file"
      and debug = flag "debug" (no_arg)
                  ~doc:"should print the debugging statements"
      in

        run_txn spec "/simple" ~f:(main trans id name debug)
    ]
    |> Command.run