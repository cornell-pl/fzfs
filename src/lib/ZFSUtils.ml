open Core
open Async
open Types

(* TODOS:
 * - Possibly move Commands, Path, Etc into modules
 * - Make FS connect to real FS
 * -- STARE HARD AT the FS updating parts of TxZFS in ZFS.ml
 * - Make sure all of this stuff is necessary...
 * - See apath_eval for non-locality annoyance
 *)


let debug = ref false
let set_debug () = debug := true

let p format = Core.Printf.ksprintf (Core.Printf.printf "%s: %s\n%!" (Pid.to_string (Unix.getpid ()))) format

let d format = if !debug then p format else Core.Printf.ksprintf ignore format

let value_or_fail message = function
  | None -> failwith message
  | Some x -> x

let failwith format = Printf.ksprintf failwith format

let mk_ok = Result.return

let mk_err_str s = Result.fail (IllegalAction s)

let mk_err format = Printf.ksprintf mk_err_str format


let info_message ?id smsg input =
  let input = Core.String.rstrip ~drop:((=) '\n') input in
  match id with
  | None -> print_endline (Printf.sprintf "%s - %s" smsg input)
  | Some id -> print_endline (Printf.sprintf "%s - %s: %s" smsg (Writer.Id.to_string id) input)


let read_marshal_and_run reader ~init~f =
  Reader.read_marshal reader
  >>= function
  | `Eof -> return init
  | `Ok command -> f command

let write_struct = Writer.write_marshal ~flags:[]

module FS = struct
  type fs = (string, fetchRes) Hashtbl.t

  let student_fs = Hashtbl.create (module String)
  let root = "/"

  let fetch path =
    match Hashtbl.find student_fs path with
    | None -> failwith "FS.fetch: Path %s did not exist" path
    | Some x -> x

  let set = Hashtbl.set student_fs
  let update key data = set ~key ~data
  let add_child path name node =
    match Hashtbl.find student_fs path with
    | None -> failwith "FS.add_child: Path %s did not exist" path
    | Some (FFile _) -> failwith "FS.add_child: Path %s was not a directory" path
    | Some (FDir lst) ->
      update path (FDir (List.merge lst [name] ~compare:String.compare));
      update (Filename.concat path name) node

  let rem_child path name =
    match Hashtbl.find student_fs path with
    | None -> failwith "FS.rem_child: Path %s did not exist" path
    | Some (FFile _) -> failwith "FS.rem_child: Path %s was not a directory" path
    | Some (FDir lst) ->
      if List.exists lst ~f:(String.equal name)
      then
        begin
          (* Remove all subpaths *)
          Hashtbl.filter_keys_inplace student_fs
            ~f:(fun subpath ->
              not (String.is_prefix subpath ~prefix:(Filename.concat path name))
            );
          let newlst = List.filter ~f:(Fn.compose not (String.equal name)) lst in
          update path (FDir newlst)
        end
      else failwith "FS.rem_child: Child %s was not in directory" name

  let init _ =
    let student_map =
      Int.Map.of_alist_exn [
        (1,"aaa17");
        (2,"bcd97");
        (3,"fcq62");
        (4,"hvh01");
        (5,"jd753");
        (6,"jnf27");
        (7,"pab93");
        (8,"wfx20");
        (9,"yt497");
        (10,"zzz23");
      ]
    in
    let empty_dir = FDir [] in
    let grades = Filename.concat root "grades" in
    let hw1 = Filename.concat grades "hw1" in
    let get_stud = Map.find_exn student_map in
    let get_stud_p num = Filename.concat hw1 (get_stud num) in

    let simple = Filename.concat root "simple" in
    let dir = Filename.concat simple "dir" in

    let shelter = Filename.concat root "shelter" in
    let cats = Filename.concat shelter "cats" in
    let munchkin = Filename.concat cats "munchkin" in
    let other_cats = Filename.concat cats "other" in
    let dogs = Filename.concat shelter "dogs" in
    let large_dogs = Filename.concat dogs "large" in
    let small_dogs = Filename.concat dogs "small" in
    let animals = [(munchkin,
                      [("lilly", "1,tabby,1,2");
                       ("boo", "1,bombay,1,0")]);
                   (other_cats,
                      [("fiona", "0,siemese,1,3");
                       ("bell", "1,american shorthair,1,0")]);
                   (large_dogs,
                      [("ralph", "1,samoyed,1,1");
                       ("jake", "1,labrador,1,4");
                       ("callie", "1,australian sheep dog,0,2")]);
                   (small_dogs,
                      [("ally", "1,doxon,1,6")])
                   ]
    in

    let quux = Filename.concat root "quux" in

    let rec add_animals all_animals =
      let rec adda species animals =
        match animals with
        | [] -> ()
        | (name, info)::t -> add_child species name (FFile info); adda species t
      in
      match all_animals with
      | [] -> ()
      | (species, animals)::t -> adda species animals; add_animals t
    in
    Hashtbl.set student_fs ~key:root ~data:empty_dir;
    add_child root "grades" empty_dir;
    add_child grades "hw1" empty_dir;
    add_child hw1 (get_stud 1) empty_dir;
    add_child (get_stud_p 1) "1" (FFile "0");
    add_child (get_stud_p 1) "2" (FFile "5");
    add_child (get_stud_p 1) "3" (FFile "3");
    add_child hw1 (get_stud 2) empty_dir;
    add_child (get_stud_p 2) "1" (FFile "0");
    add_child (get_stud_p 2) "2" (FFile "0");
    add_child (get_stud_p 2) "3" (FFile "0");
    add_child hw1 (get_stud 3) empty_dir;
    add_child (get_stud_p 3) "1" (FFile "4");
    add_child (get_stud_p 3) "2" (FFile "9");
    add_child (get_stud_p 3) "3" (FFile "10");
    add_child root "shelter" empty_dir;
    add_child shelter "cats" empty_dir;
    add_child shelter "dogs" empty_dir;
    add_child cats "munchkin" empty_dir;
    add_child cats "other" empty_dir;
    add_child dogs "large" empty_dir;
    add_child dogs "small" empty_dir;
    add_animals animals;
    add_child root "simple" empty_dir;
    add_child simple "index.txt" (FFile "a\nb\nc\nd");
    add_child simple "dir" empty_dir;
    add_child dir "a" (FFile "apple");
    add_child dir "b" (FFile "banana");
    add_child dir "c" (FFile "carrot");
    add_child dir "d" (FFile "dragon fruit");
    add_child dir "e" (FFile "egg plant");

    add_child root "quux" empty_dir;
    add_child quux "a" (FFile "c");
    add_child quux "c" (FFile "Hello World");



end

(* Gets the string representation of a path from a zipper *)
let rec get_path_s z =
  let (name,_) = z.current in
  match z.ancestor with
  | None -> name
  | Some ancestor -> Filename.concat (get_path_s ancestor) name

let rec path_of_list lst =
  let str_to_path = function
  | "." -> CDir
  | ".." -> PDir
  | "/" -> Root
  | s -> Name s
  in
  match lst with
  | [] -> CDir
  | hd :: [] -> str_to_path hd
  | hd :: tl -> PConcat(str_to_path hd, path_of_list tl)

let path_of_string = Fn.compose path_of_list Filename.parts


(* Gets the path of a zipper, with the concatenations stacked right *)
let get_path_p z =
  let rec collect_names z =
    let (name,_) = z.current in
    match z.ancestor with
    | None -> []
    | Some ancestor -> name :: (collect_names ancestor)
  in
  PConcat(Root, collect_names z |> List.rev |> path_of_list)

let get_name z = fst z.current

let force z =
  match z.current with
  | (name,Unforced) ->
    let t =
      match FS.fetch (get_path_s z) with
      | FDir lst -> Dir (List.map ~f:(fun name -> (name,Unforced)) lst)
      | FFile contents -> File contents
    in
    {z with current = (name,t)}
  | _ -> z

let fetch z =
  match snd z.current with
  | Dir lst -> FDir (List.map ~f:fst lst)
  | File contents -> FFile contents
  | Unforced -> failwith "fetch: Node was unforced"

let fetch_unforced z =
  try
    fetch z
  with
    | Failure _ -> force z |> fetch

let seq f g = Fn.flip Fn.compose f g

let get_dir = seq fetch_unforced (function
  | FDir l -> l
  | _ -> failwith "get_dir: current node was not a directory")

let is_dir = seq fetch_unforced (function | FDir _ -> true | _ -> false)

let has_child s = seq fetch_unforced
  (function | FDir l when List.exists ~f:(String.equal s) l -> true | _ ->
  false)

let fs_of_res = function
  | FDir [] -> Dir []
  | FFile s -> File s
  | _ -> failwith "fs_of_res: Requires an empty directory or a file"

(* Command helpers *)

let rec seq_of_list = function
  | [] -> Skip
  | hd :: [last] -> Seq(hd,last)
  | hd :: tl -> Seq(hd,seq_of_list tl)

let rec list_of_seq = function
 | Seq (c1,c2) -> list_of_seq c1 @ list_of_seq c2
 | c -> [c]

let flatten_seq = Fn.compose seq_of_list list_of_seq

(* Path interaction *)

let rec goto_root = function
  | {ancestor = Some z;_} -> Seq(Up,goto_root z)
  | _ -> Skip

let rec path_to_command z = function
  | Name name -> GotoChild name
  | CDir -> Skip
  | PDir -> Up
  | Root -> goto_root z
  | PConcat (p1,p2) -> Seq(path_to_command z p1,path_to_command z p2)

let path_eval = path_to_command

let msg_of_context ~get_zipper = function
  | Ok context -> FSZip (get_zipper context)
  | Error fsErr -> FSErr fsErr

(* TODO: This is kinda non-local, which is annoying, if not currently critical.
 * It would be nice to do the inverse of the path, but this is remarkably
 * difficult to compute with the current constructs.
 * In particular, it's not obvious how to reverse PDir
 * (or Root if it's not restricted to appear at the start)
 *)
let apath_eval z = path_to_command z (get_path_p z)