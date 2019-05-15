
open Core
open Async

module LeafType = struct
  type leafType = string list
  let to_string = String.concat ~sep:"\n"
end
                    
let _ = DTZ.set_debug ()
                  
module DTZ = DTZ.Make(LeafType)
open DTZ
               
let pathTree =
  let current_dir = Core.Sys.getcwd () in
  let file = current_dir ^/ "baby/names/yob1934.txt" in 
  match Core.Sys.is_file file with
  | `Yes -> PathLeaf (file,"localhost")
  | _ -> failwith "This executable should only be run in the examples directory"

let parser path =
  Connection.Leaf (Core.In_channel.with_file path ~f:(Core.In_channel.input_lines ~fix_win_eol:false))
  
                  
(* Actual running code *)    
let main explore () =
  distribute (PathDesc (pathTree,parser)) >>> fun connection  ->
  let defer =
    if explore
    then Connection.explore connection
    else failwith "Rest not implemented"
  in
  defer >>> DTZ.shutdown
  
                
let command =
  Core.Command.basic  ~summary:"Baby Name Example"
    (let open Command.Let_syntax in
     [%map_open
      let explore = flag "-explore" no_arg ~doc:"Start baby name example in exploration mode" in
          main explore
     ]
    )

let servers = ["localhost"]
    
let () = start command servers
