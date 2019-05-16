%{ open ForestTypes
   open Location
   (*type forest_token = token
   *)
   let make_loc p1 p2 =
     { loc_start = p1;
       loc_end = p2;
       loc_ghost = false;
     }
%}

(* General *)
%token <string> ID
%token LBRACE
%token RBRACE
%token LBRACK
%token RBRACK
%token LPAREN
%token RPAREN
%token EOF
%token SEMICOLON
%token EQ
%token BAR


(* T+F *)
%token FILE
(* %token LINK *)
(* %token OPT *)

(* Forest Specific *)
%token <string> STRING
%token <string> AQUOT
%token DIR
%token PADS
%token IS
%token MATCHES
(* %token WHERE *)
%token DCOLON
%token BARROW



(* Forest *)

%type <ForestTypes.specification ForestTypes.ast> f_ast f_cons f_path_ast f_bast f_cons_ast f_last direntry
%type <ForestTypes.exp> path_typ gen_statement



(* Starts *)

%start <(ForestTypes.var * ForestTypes.specification ForestTypes.ast) list> forest_prog
%%


(* Forest parsing *)
forest_prog: l = nonempty_list(separated_pair(ID,EQ,f_ast)); EOF  { l } ;

f_ast:
  (* TODO: where not yet implemented
  | f = f_path_ast; WHERE; s = AQUOT
   { let l = make_loc $startpos(f) $endpos(s) in
      mk_ast l @@ Predicate (f,s) }
  *)
  | f_path_ast { $1 }
  ;



f_path_ast:
  | p = path_typ; DCOLON ; f=f_path_ast
   { let l = make_loc $startpos(p) $endpos(f) in
     mk_ast l @@ PathExp (p,f) }
  | f_bast { $1 }
  ;

f_bast:
  (* TODO: add back in when options are implmented
  | f = f_bast; OPT
   { let l = make_loc $startpos(f) $endpos($2) in
     mk_ast l @@ Option (f) }
  *)
  | f_cons_ast { $1 }
  ;

f_cons_ast:
  | f_cons { $1 }
  | f_last { $1 }
  ;

f_last:
  | DIR; LBRACE; entry = direntry ; RBRACE (*TODO: Figure out a way to allow a final ; *)
     { entry }
  | LBRACK; f = f_ast; BAR ; x = ID ; BARROW; e = gen_statement ; RBRACK
     { let l = make_loc $startpos($1) $endpos($5) in (*TODO: Make sure you want symbolstartpos here *)
       mk_ast l @@ Comp (f,x,e) }
  | x = ID
     { let l = make_loc $startpos(x) $endpos(x) in
       mk_ast l @@ Var (x) }
  | LPAREN; f = f_ast; RPAREN { f }
  ;

f_cons:
  | FILE
     { let l = make_loc $startpos $endpos in
       mk_ast l File }
  (* TODO: add this back if links are implemented
  | LINK
     { let l = make_loc $startpos $endpos in
       mk_ast l Link }
  *)
  | PADS; x = ID
     { let l = make_loc $startpos($1) $endpos(x) in
       mk_ast l @@ Pads(x) }
  ;

path_typ:
  | STRING
    { let l = make_loc $startpos $endpos in
      Name(l,$1) }
  | ID
     { let l = make_loc $startpos $endpos in
       Var(l,$1) }
  (* TODO: from oforest, may add back in
  | AQUOT
     { let l = make_loc $startpos $endpos in
       OC_Path(l,$1) }
  *)
  ;


gen_statement:
  | MATCHES; reg = STRING
     { let l = make_loc $startpos($1) $endpos(reg) in
       Matches(l,reg) }
  | AQUOT
      { let l = make_loc $startpos $endpos in
        InList(l,$1) }
  | ID
      {
        let l = make_loc $startpos $endpos in
        InVar(l,$1)
      }
  ;


direntry:
  | x =ID ; IS ; f = f_ast; SEMICOLON; r = direntry {
    let l = make_loc $startpos(x) $endpos(r) in
      mk_ast l @@ DPair (x, f, r)
  }
  | ID ; IS ; f = f_ast ; SEMICOLON {
    (*TODO: if goto can goto something in a dpair then this needs to be changed*)
    f
  }
  | ID ; IS ; f = f_ast {
    (*TODO: if goto can goto something in a dpair then this needs to be changed*)
    f
  }
