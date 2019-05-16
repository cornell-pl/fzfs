open Migrate_parsetree
open Ast_mapper
open Parsetree
open Parser_helper
open ForestTypes

let debug = false

let d f =
  if debug then f ()

(*extracts the portions that use the txforest extention from the ocaml ast
 * parses them and connverts the forest syntax into an ocaml ast*)
let forest_str_mapper mapper (strl: structure) : structure =
      (*converts one txforest extention structure item into a list of ocaml structures*)
      let forest_replace (acc: structure list) (str: structure_item) : structure list  =
        match str with
        | { pstr_desc = Pstr_extension
            (({txt = "txforest"; _}, PStr
              [{pstr_desc = Pstr_eval ({
                  pexp_desc = Pexp_constant (Pconst_string (fstr, Some ""));
                  pexp_loc;_}
                ,_);_
              }]
             ),_)
          ; pstr_loc ; _} ->

          let forest_ast = Parser_helper.ParserHelper.forest_parse_string pexp_loc fstr in
          d (fun _ ->
            List.iter
              (fun (_, s) ->
                print_string
                (show_ast
                  (fun f s' ->
                    Format.pp_print_string f (show_specification s')
                  )
                  s
                )
              )
              forest_ast;
            print_string "\n"
          );


          let ocaml_asts : Parsetree.structure = PpxTXForestLib.def_generator pstr_loc forest_ast in
            let _ = pstr_loc, forest_ast, ocaml_asts in
              d (fun _ -> Pprintast.structure Format.std_formatter ocaml_asts);
              ocaml_asts :: acc

         | x -> [default_mapper.structure_item mapper x] :: acc

      in
      List.flatten (List.rev (List.fold_left forest_replace [] strl))


(* [txforest_mapper config cookies] creates a mapper documentation here:
 * https://docs.mirage.io/ocaml-migrate-parsetree/Migrate_parsetree_driver/index.html *)
let txforest_mapper _ _ =
  {
    default_mapper with
    structure = forest_str_mapper;
  }



(* registers the rewriter *)
(* note this does not use ppxlib (ppx library made public in Fall 2018),
 * which has a different format for doing this
 * will look into using pxlib*)
let () = Driver.register ~name:"txforest"  Versions.ocaml_405 txforest_mapper

(* let txforest_transform =
  Extension.declare "txforest" Structure_item (*pattern*) (*conversion function*)
let () = Driver.register_transformation "txforest" ~rules: [ Context_free.Rule.extension txforest_transform ] *)
