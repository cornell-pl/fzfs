open Parser_helper
open ForestTypes
open Parsetree
open Ast_helper


(*helper functions -- there are more where these come from in the
 * oforest repository in the parsing/utilies file*)

let get_NodeAndLoc (ast : 'a ast) : ('a * loc) = ast.node, ast.loc

let make_ast_lid loc (name : string) : Longident.t Asttypes.loc =
  {txt = (Longident.parse name); loc}

let make_ast_str loc (name : string) : string Asttypes.loc =
  {txt = name; loc = loc}

let exp_make_ident loc (name : string) : Parsetree.expression =
  Exp.ident ~loc (make_ast_lid loc name)

let exp_make_string loc (str : string) : Parsetree.expression =
  Exp.constant ~loc (Const.string str)

let exp_make_ocaml loc (str : string) : Parsetree.expression =
  let open Lexing in
  let open Location in
  let lexbuf = Lexing.from_string str in
  let lexbuf = {lexbuf with
    lex_start_p = loc.loc_start;
    lex_curr_p = loc.loc_start;
  } in
  Parse.expression lexbuf

let pat_make_var loc (name : string) : Parsetree.pattern =
  Pat.var ~loc (make_ast_str loc name)


(*[def_generator loc flist] generates ocaml ast (structure) from the tx-forest ast [fast]
 * the location of this structure (list of let statements) is given by [loc]*)
let def_generator loc (flist : (var * specification ast) list) : structure =
  (*[def_gen fast] converts one txforest structure item [fast] into the
   * ocmal ast for that expresion*)
  let rec def_gen (fast : specification ast) :  expression =
    let (e, loc) = get_NodeAndLoc fast in
    match e with
    | ForestTypes.File ->begin
      [%expr Forest.File ][@metaloc loc]
    end
    | ForestTypes.Pads x -> begin
      let _ = x in
      failwith "unimplemented"
    end
    | ForestTypes.PathExp (e', fast') -> begin
      let _ = e', fast' in
      let oast = def_gen fast' in
      let path_expr =  match e' with
        | Name (loc', s) -> begin
          let _ = loc', s in
          let s_expr = exp_make_string loc' s in
            [%expr Forest.Name [%e s_expr]][@metaloc loc']
        end
        | Var (loc', x) -> begin
          let _ = loc', x in
          let x_expr = exp_make_string loc' x in
            [%expr Forest.Var [%e x_expr]][@metaloc loc']
        end
        | _ -> failwith "paths should only be names or vars"
      in
      let _ = oast, path_expr in
        [%expr Forest.PathExp ([%e path_expr], [%e oast]) ][@metaloc loc]

    end
    | ForestTypes.Comp (fast', x, e') -> begin
      let _ = fast', x, e' in
      let var_expr = exp_make_string loc x  in
      let e_expr = match e' with
        | Matches (loc', regex) -> begin
            let _ = loc', regex in
            [%expr Forest.DirList (fun s -> Str.string_match (Str.regexp [%e exp_make_string loc' regex]) s 0) ][@metaloc loc']
        end
        | InList (loc', a) -> begin
            let _ = loc', a in
            [%expr Forest.DirList [%e exp_make_ocaml loc' a]][@metaloc loc']
        end
        | InVar (loc', x) -> begin
            let _ = loc', x in
            [%expr Forest.VDirList
              ([%e exp_make_string loc' x],
              (fun lines s -> List.mem ~equal:(fun s1 s2 -> s1 = s2) (String.split lines ~on:'\n') s))
            ][@metaloc loc']
        end
        | _ -> failwith "comprehension expressions should be ocaml or matches"
      in
      let oast' = (def_gen fast')in
        [%expr Forest.Comp ([%e oast'], [%e var_expr] , [%e e_expr]) ][@metaloc loc]

    end
    | ForestTypes.DPair (x, fast', fast'') -> begin
      let _ = x, fast', fast'' in
      let var_expr = exp_make_string loc x  in
      let oast', oast'' = (def_gen fast'), (def_gen fast'') in
        [%expr Forest.DPair ([%e var_expr], [%e oast'] , [%e oast'']) ][@metaloc loc]
    end
    | ForestTypes.Var x -> begin
      let _ = x in
        exp_make_ident loc x
    end
  in
  (* [str_gen name expr] makes a structure item (let statement) with [name] as the the
   * name of the bound variable and [expr] the ocaml ast for the expression being
   * bound *)
  let str_gen (name: string) (expr : expression) : structure_item =
    let _ =  name in
    [%stri
       let [%p pat_make_var loc name] = [%e expr]
    ][@metaloc loc]
  in
  let _ = loc in
  let ocaml_exprs = List.map (fun (name, e) -> (name, def_gen e)) flist in
  let str_items = List.map (fun (name, e) -> str_gen name e ) ocaml_exprs in
    str_items

