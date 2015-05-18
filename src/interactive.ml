(* This file simply records interactive commands to load in all the
   files.

#use "topfind";;

#directory "/tmp/l/github/super/e3/build";;
#load "e3.cma";;

#directory "/tmp/l/github/super/p3/build";;
#mod_use "p3_gensym.ml";;
#mod_use "p3_box.ml";;
#mod_use "p3_span.ml";;
#mod_use "p3_core.ml";;
#mod_use "p3_extra.ml";;
#mod_use "p3_lib.ml";;


Or, if the library is available:

#load "p3.cma";;

Examples:

#mod_use "p3_examples.ml";;


*)
