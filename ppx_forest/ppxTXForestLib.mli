
(*[def_generator loc flist] generates ocaml ast (structure) from the tx-forest ast [fast]
 * the location of this structure (list of let statements) is given by [loc]*)
val def_generator : Ast_helper.loc -> (Parser_helper.ForestTypes.var * Parser_helper.ForestTypes.specification Parser_helper.ForestTypes.ast) list -> Parsetree.structure

