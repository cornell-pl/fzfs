# !/bin/bash
# $1 = filename

ocamlfind ocamlc -package core,async,ppx_jane,rpc_parallel -dsource $1
