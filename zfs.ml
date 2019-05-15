
open Core
open Async

(* FS basics *)
module LeafType = struct
  type leafType = string
  let to_string = Fn.id
end
                    
let _ = DTZ.set_debug ()
                  
module DTZ = DTZ.Make(LeafType)
open DTZ


                          
module Folder = String.Map
               
let simpleFS =
  let open DTZ.Connection in
  let d1 =
    let d11 = Node (Folder.set Folder.empty ~key:"d111" ~data:(Node Folder.empty)) in
    Node
      (Folder.set ~key:"fl13" ~data:(Leaf "File 3") Folder.empty
          |> Folder.set ~key:"d11" ~data:d11
      )
  in
  Node
    (Folder.of_alist_exn
       [
         ("d1",d1);
         ("d2",Node Folder.empty);
         ("fl1", Leaf "File 1");
         ("fl2", Leaf "File 2");
       ]
    )

(* Actual running code *)    
let main explore () =
  distribute (RealDesc (simpleFS, "/")) >>> fun connection  ->
  let defer =
    if explore
    then Connection.explore connection
    else failwith "Rest not implemented"
  in
  defer >>> DTZ.shutdown
  
                
let command =
  Core.Command.basic  ~summary:"Start file system server"
    (let open Command.Let_syntax in
     [%map_open
      let explore = flag "-explore" no_arg ~doc:"Start ZFS in exploration mode" in
          main explore
     ]
    )

let servers = ["localhost"]
    
let () = start command servers
