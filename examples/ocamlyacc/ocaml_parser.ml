(*

#require "compiler-libs";;
#require "compiler-libs.common";;

*)

let time f x =
    let start = Unix.gettimeofday ()
    in let res = f x
    in let stop = Unix.gettimeofday ()
    in let () = Printf.printf "Execution time: %fs\n%!" (stop -. start)
    in
       res


(* p3pre.ml *)

open P3_lib
open P3_everything

(* command line args *)
type ty_cl_args = { input:string; debug:bool }
let cl0 = { 
  input="/tmp/test_ml2"; (* default to stdin *)
  debug=false
}
  
(* precedence to earlier args *)
let rec parse_CL = 
  let open P1_lib.P1_core.BasicParsers in
  fun i -> (
  let f1 (f,xs) cl = (match (f,xs) with
    | ("-f",[a]) -> {cl with input=a }
    | ("-debug",[]) -> {cl with debug=true } 
    | _ -> (failwith ("parse_CL: unrecognized flag/arg combination: "^f^" "^(String.concat " " xs))))
  in
  let sep = a "\x00" in
  (((listof parse_FLARGS sep) **> parse_EOF) >> (fun (xs,_) -> itlist f1 xs cl0))) i

let args = get_args parse_CL Sys.argv

(* fix debug at this point *)
let _ = (debugging:=args.debug)

(* our terminal parsers must take a token and check whether it is of the form required; e.g. the terminal parser ?DOTDOT? must check that the token is DOTDOT *)

(* tokens with arguments are treated slightly differently *)

open Parser

let tokint_of_token tok = (match tok with
  | AMPERAMPER              ->     1
  | AMPERSAND               ->     2
  | AND                     ->     3
  | AS                      ->     4
  | ASSERT                  ->     5
  | BACKQUOTE               ->     6
  | BANG                    ->     7
  | BAR                     ->     8
  | BARBAR                  ->     9
  | BARRBRACKET             ->    10
  | BEGIN                   ->    11
  | CHAR(_)                 ->    12
  | CLASS                   ->    13
  | COLON                   ->    14
  | COLONCOLON              ->    15
  | COLONEQUAL              ->    16
  | COLONGREATER            ->    17
  | COMMA                   ->    18
  | CONSTRAINT              ->    19
  | DO                      ->    20
  | DONE                    ->    21
  | DOT                     ->    22
  | DOTDOT                  ->    23
  | DOWNTO                  ->    24
  | ELSE                    ->    25
  | END                     ->    26
  | EOF                     ->    27
  | EQUAL                   ->    28
  | EXCEPTION               ->    29
  | EXTERNAL                ->    30
  | FALSE                   ->    31
  | FLOAT(_)                ->    32
  | FOR                     ->    33
  | FUN                     ->    34
  | FUNCTION                ->    35
  | FUNCTOR                 ->    36
  | GREATER                 ->    37
  | GREATERRBRACE           ->    38
  | GREATERRBRACKET         ->    39
  | IF                      ->    40
  | IN                      ->    41
  | INCLUDE                 ->    42
  | INFIXOP0(_)             ->    43
  | INFIXOP1(_)             ->    44
  | INFIXOP2(_)             ->    45
  | INFIXOP3(_)             ->    46
  | INFIXOP4(_)             ->    47
  | INHERIT                 ->    48
  | INITIALIZER             ->    49
  | INT (_)                 ->    50
  | INT32 (_)               ->    51
  | INT64 (_)               ->    52
  | LABEL (_)               ->    53
  | LAZY                    ->    54
  | LBRACE                  ->    55
  | LBRACELESS              ->    56
  | LBRACKET                ->    57
  | LBRACKETBAR             ->    58
  | LBRACKETLESS            ->    59
  | LBRACKETGREATER         ->    60
(*  | LBRACKETPERCENT         ->    61
  | LBRACKETPERCENTPERCENT  ->    62 *)
  | LESS                    ->    63
  | LESSMINUS               ->    64
  | LET                     ->    65
  | LIDENT (_)              ->    66
  | LPAREN                  ->    67
(*  | LBRACKETAT              ->    68
  | LBRACKETATAT            ->    69 *)
  | MATCH                   ->    70
  | METHOD                  ->    71
  | MINUS                   ->    72
  | MINUSDOT                ->    73
  | MINUSGREATER            ->    74
  | MODULE                  ->    75
  | MUTABLE                 ->    76
  | NATIVEINT (_)           ->    77
  | NEW                     ->    78
  | OBJECT                  ->    79
  | OF                      ->    80
  | OPEN                    ->    81
  | OPTLABEL (_)            ->    82
  | OR                      ->    83
(*  | PERCENT                 ->    84 *)
  | PLUS                    ->    85
  | PLUSDOT                 ->    86
  | PREFIXOP (_)            ->    87
  | PRIVATE                 ->    88
  | QUESTION                ->    89
  | QUOTE                   ->    90
  | RBRACE                  ->    91
  | RBRACKET                ->    92
  | REC                     ->    93
  | RPAREN                  ->    94
  | SEMI                    ->    95
  | SEMISEMI                ->    96
  | SHARP                   ->    97
  | SIG                     ->    98
  | STAR                    ->    99
  | STRING (_)              ->   100
  | STRUCT                  ->   101
  | THEN                    ->   102
  | TILDE                   ->   103
  | TO                      ->   104
  | TRUE                    ->   105
  | TRY                     ->   106
  | TYPE                    ->   107
  | UIDENT (_)              ->   108
  | UNDERSCORE              ->   109
  | VAL                     ->   110
  | VIRTUAL                 ->   111
  | WHEN                    ->   112
  | WHILE                   ->   113
  | WITH                    ->   114
  | COMMENT (_)             ->   115)

let tokint_of_string s = (match s with
  | "?eps?"                 ->    -1
  | "?AMPERAMPER?"          ->     1
  | "?AMPERSAND?"           ->     2
  | "?AND?"                 ->     3
  | "?AS?"                  ->     4
  | "?ASSERT?"              ->     5
  | "?BACKQUOTE?"           ->     6
  | "?BANG?"                ->     7
  | "?BAR?"                 ->     8
  | "?BARBAR?"              ->     9
  | "?BARRBRACKET?"         ->    10
  | "?BEGIN?"               ->    11
  | "?CHAR?"                ->    12
  | "?CLASS?"               ->    13
  | "?COLON?"               ->    14
  | "?COLONCOLON?"          ->    15
  | "?COLONEQUAL?"          ->    16
  | "?COLONGREATER?"        ->    17
  | "?COMMA?"               ->    18
  | "?CONSTRAINT?"          ->    19
  | "?DO?"                  ->    20
  | "?DONE?"                ->    21
  | "?DOT?"                 ->    22
  | "?DOTDOT?"              ->    23
  | "?DOWNTO?"              ->    24
  | "?ELSE?"                ->    25
  | "?END?"                 ->    26
  | "?EOF?"                 ->    27
  | "?EQUAL?"               ->    28
  | "?EXCEPTION?"           ->    29
  | "?EXTERNAL?"            ->    30
  | "?FALSE?"               ->    31
  | "?FLOAT?"               ->    32
  | "?FOR?"                 ->    33
  | "?FUN?"                 ->    34
  | "?FUNCTION?"            ->    35
  | "?FUNCTOR?"             ->    36
  | "?GREATER?"             ->    37
  | "?GREATERRBRACE?"       ->    38
  | "?GREATERRBRACKET?"     ->    39
  | "?IF?"                  ->    40
  | "?IN?"                  ->    41
  | "?INCLUDE?"             ->    42
  | "?INFIXOP0?"            ->    43
  | "?INFIXOP1?"            ->    44
  | "?INFIXOP2?"            ->    45
  | "?INFIXOP3?"            ->    46
  | "?INFIXOP4?"            ->    47
  | "?INHERIT?"             ->    48
  | "?INITIALIZER?"         ->    49
  | "?INT?"                 ->    50
  | "?INT32?"               ->    51
  | "?INT64?"               ->    52
  | "?LABEL?"               ->    53
  | "?LAZY?"                ->    54
  | "?LBRACE?"              ->    55
  | "?LBRACELESS?"          ->    56
  | "?LBRACKET?"            ->    57
  | "?LBRACKETBAR?"         ->    58
  | "?LBRACKETLESS?"        ->    59
  | "?LBRACKETGREATER?"     ->    60
  | "?LBRACKETPERCENT?"     ->    61
  | "?LBRACKETPERCENTPERCENT?" -> 62
  | "?LESS?"                ->    63
  | "?LESSMINUS?"           ->    64
  | "?LET?"                 ->    65
  | "?LIDENT?"              ->    66
  | "?LPAREN?"              ->    67
  | "?LBRACKETAT?"          ->    68
  | "?LBRACKETATAT?"        ->    69
  | "?MATCH?"               ->    70
  | "?METHOD?"              ->    71
  | "?MINUS?"               ->    72
  | "?MINUSDOT?"            ->    73
  | "?MINUSGREATER?"        ->    74
  | "?MODULE?"              ->    75
  | "?MUTABLE?"             ->    76
  | "?NATIVEINT?"           ->    77
  | "?NEW?"                 ->    78
  | "?OBJECT?"              ->    79
  | "?OF?"                  ->    80
  | "?OPEN?"                ->    81
  | "?OPTLABEL?"            ->    82
  | "?OR?"                  ->    83
  | "?PERCENT?"             ->    84
  | "?PLUS?"                ->    85
  | "?PLUSDOT?"             ->    86
  | "?PREFIXOP?"            ->    87
  | "?PRIVATE?"             ->    88
  | "?QUESTION?"            ->    89
  | "?QUOTE?"               ->    90
  | "?RBRACE?"              ->    91
  | "?RBRACKET?"            ->    92
  | "?REC?"                 ->    93
  | "?RPAREN?"              ->    94
  | "?SEMI?"                ->    95
  | "?SEMISEMI?"            ->    96
  | "?SHARP?"               ->    97
  | "?SIG?"                 ->    98
  | "?STAR?"                ->    99
  | "?STRING?"              ->   100
  | "?STRUCT?"              ->   101
  | "?THEN?"                ->   102
  | "?TILDE?"               ->   103
  | "?TO?"                  ->   104
  | "?TRUE?"                ->   105
  | "?TRY?"                 ->   106
  | "?TYPE?"                ->   107
  | "?UIDENT?"              ->   108
  | "?UNDERSCORE?"          ->   109
  | "?VAL?"                 ->   110
  | "?VIRTUAL?"             ->   111
  | "?WHEN?"                ->   112
  | "?WHILE?"               ->   113
  | "?WITH?"                ->   114
  | "?COMMENT?"             ->   115
  | _ -> (failwith ("tokint_of_string: unknown terminal: "^s)))

let mk_term_parser s0 = (
  let eps = tokint_of_string "?eps?" in
  let s0 = tokint_of_string s0 in
  fun (SS(s,i,j)) -> (
    if s0 = eps then [SS(s,i,i)] else
    (* if s0 = "?FIXME?" then [] else we don't use this terminal parser any more? *)
    if i<j then (
      let n = (* tokint_of_token *) (Array.get s i) in
      if (s0=n) then 
        [SS(s,i,i+1)]
      else
        [])
    else
      []))

let term_to_parser s = mktmparser s (mk_term_parser s)


 let fIXME = fun x -> failwith "FIXME" 
(* p3mid.ml *)

let tbl_IMPLEMENTATION = MyHashtbl.create 10
let tbl_INTERFACE = MyHashtbl.create 10
let tbl_TOPLEVEL_PHRASE = MyHashtbl.create 10
let tbl_TOP_STRUCTURE = MyHashtbl.create 10
let tbl_TOP_STRUCTURE_TAIL = MyHashtbl.create 10
let tbl_USE_FILE = MyHashtbl.create 10
let tbl_USE_FILE_TAIL = MyHashtbl.create 10
let tbl_PARSE_CORE_TYPE = MyHashtbl.create 10
let tbl_PARSE_EXPRESSION = MyHashtbl.create 10
let tbl_PARSE_PATTERN = MyHashtbl.create 10
let tbl_STRUCTURE_TAIL = MyHashtbl.create 10
let tbl_STR_ATTRIBUTE = MyHashtbl.create 10
let tbl_STRUCTURE_ITEM = MyHashtbl.create 10
let tbl_MODULE_BINDINGS = MyHashtbl.create 10
let tbl_MODULE_BINDING = MyHashtbl.create 10
let tbl_SIGNATURE = MyHashtbl.create 10
let tbl_SIGNATURE_TAIL = MyHashtbl.create 10
let tbl_SIG_ATTRIBUTE = MyHashtbl.create 10
let tbl_SIGNATURE_ITEM = MyHashtbl.create 10
let tbl_MODULE_DECLARATION = MyHashtbl.create 10
let tbl_MODULE_REC_DECLARATIONS = MyHashtbl.create 10
let tbl_MODULE_REC_DECLARATION = MyHashtbl.create 10
let tbl_MODULE_TYPE = MyHashtbl.create 10
let tbl_CLASS_DECLARATIONS = MyHashtbl.create 10
let tbl_CLASS_DECLARATION = MyHashtbl.create 10
let tbl_CLASS_FUN_BINDING = MyHashtbl.create 10
let tbl_CLASS_FUN_DEF = MyHashtbl.create 10
let tbl_CLASS_SIMPLE_EXPR = MyHashtbl.create 10
let tbl_CLASS_SELF_PATTERN = MyHashtbl.create 10
let tbl_CLASS_FIELDS = MyHashtbl.create 10
let tbl_CLASS_EXPR = MyHashtbl.create 10
let tbl_CLASS_FIELD = MyHashtbl.create 10
let tbl_PARENT_BINDER = MyHashtbl.create 10
let tbl_VALUE = MyHashtbl.create 10
let tbl_METHOD_ = MyHashtbl.create 10
let tbl_CLASS_SIG_BODY = MyHashtbl.create 10
let tbl_CLASS_SELF_TYPE = MyHashtbl.create 10
let tbl_CLASS_SIG_FIELDS = MyHashtbl.create 10
let tbl_CLASS_SIG_FIELD = MyHashtbl.create 10
let tbl_VALUE_TYPE = MyHashtbl.create 10
let tbl_CONSTRAIN_FIELD = MyHashtbl.create 10
let tbl_CLASS_DESCRIPTIONS = MyHashtbl.create 10
let tbl_CLASS_DESCRIPTION = MyHashtbl.create 10
let tbl_CLASS_TYPE = MyHashtbl.create 10
let tbl_CLASS_TYPE_DECLARATIONS = MyHashtbl.create 10
let tbl_CLASS_TYPE_DECLARATION = MyHashtbl.create 10
let tbl_CLASS_TYPE_PARAMETERS = MyHashtbl.create 10
let tbl_CLASS_SIGNATURE = MyHashtbl.create 10
let tbl_PATTERN_VAR = MyHashtbl.create 10
let tbl_OPT_DEFAULT = MyHashtbl.create 10
let tbl_LABEL_LET_PATTERN = MyHashtbl.create 10
let tbl_LABEL_VAR = MyHashtbl.create 10
let tbl_LET_PATTERN = MyHashtbl.create 10
let tbl_MODULE_BINDING_BODY = MyHashtbl.create 10
let tbl_CLASS_STRUCTURE = MyHashtbl.create 10
let tbl_MODULE_EXPR = MyHashtbl.create 10
let tbl_SIMPLE_LABELED_EXPR_LIST = MyHashtbl.create 10
let tbl_LABELED_SIMPLE_EXPR = MyHashtbl.create 10
let tbl_LABEL_EXPR = MyHashtbl.create 10
let tbl_LABEL_IDENT = MyHashtbl.create 10
let tbl_LET_BINDINGS = MyHashtbl.create 10
let tbl_LET_BINDING = MyHashtbl.create 10
let tbl_LIDENT_LIST = MyHashtbl.create 10
let tbl_LET_BINDING_ = MyHashtbl.create 10
let tbl_STRICT_BINDING = MyHashtbl.create 10
let tbl_FUN_BINDING = MyHashtbl.create 10
let tbl_MATCH_CASES = MyHashtbl.create 10
let tbl_MATCH_CASE = MyHashtbl.create 10
let tbl_LABELED_SIMPLE_PATTERN = MyHashtbl.create 10
let tbl_FUN_DEF = MyHashtbl.create 10
let tbl_EXPR_COMMA_LIST = MyHashtbl.create 10
let tbl_SIMPLE_EXPR = MyHashtbl.create 10
let tbl_RECORD_EXPR = MyHashtbl.create 10
let tbl_LBL_EXPR_LIST = MyHashtbl.create 10
let tbl_LBL_EXPR = MyHashtbl.create 10
let tbl_FIELD_EXPR_LIST = MyHashtbl.create 10
let tbl_EXPR_SEMI_LIST = MyHashtbl.create 10
let tbl_EXPR = MyHashtbl.create 10
let tbl_TYPE_CONSTRAINT = MyHashtbl.create 10
let tbl_SIMPLE_PATTERN = MyHashtbl.create 10
let tbl_SIMPLE_PATTERN_NOT_IDENT = MyHashtbl.create 10
let tbl_PATTERN_COMMA_LIST = MyHashtbl.create 10
let tbl_PATTERN_SEMI_LIST = MyHashtbl.create 10
let tbl_LBL_PATTERN_LIST = MyHashtbl.create 10
let tbl_LBL_PATTERN = MyHashtbl.create 10
let tbl_PRIMITIVE_DECLARATION = MyHashtbl.create 10
let tbl_TYPE_DECLARATIONS = MyHashtbl.create 10
let tbl_TYPE_DECLARATION = MyHashtbl.create 10
let tbl_CONSTRAIN = MyHashtbl.create 10
let tbl_TYPE_KIND = MyHashtbl.create 10
let tbl_OPTIONAL_TYPE_PARAMETERS = MyHashtbl.create 10
let tbl_OPTIONAL_TYPE_PARAMETER_LIST = MyHashtbl.create 10
let tbl_OPTIONAL_TYPE_PARAMETER = MyHashtbl.create 10
let tbl_TYPE_VARIANCE = MyHashtbl.create 10
let tbl_TYPE_PARAMETER_LIST = MyHashtbl.create 10
let tbl_TYPE_PARAMETER = MyHashtbl.create 10
let tbl_CONSTRUCTOR_DECLARATIONS = MyHashtbl.create 10
let tbl_EXCEPTION_DECLARATION = MyHashtbl.create 10
let tbl_CONSTRUCTOR_DECLARATION = MyHashtbl.create 10
let tbl_GENERALIZED_CONSTRUCTOR_ARGUMENTS = MyHashtbl.create 10
let tbl_LABEL_DECLARATIONS = MyHashtbl.create 10
let tbl_LABEL_DECLARATION = MyHashtbl.create 10
let tbl_WITH_CONSTRAINTS = MyHashtbl.create 10
let tbl_CONSTRAINTS = MyHashtbl.create 10
let tbl_TYPE_PARAMETERS = MyHashtbl.create 10
let tbl_WITH_CONSTRAINT = MyHashtbl.create 10
let tbl_WITH_TYPE_BINDER = MyHashtbl.create 10
let tbl_TYPEVAR_LIST = MyHashtbl.create 10
let tbl_CORE_TYPE2 = MyHashtbl.create 10
let tbl_SIMPLE_CORE_TYPE2 = MyHashtbl.create 10
let tbl_PACKAGE_TYPE = MyHashtbl.create 10
let tbl_PACKAGE_TYPE_CSTR = MyHashtbl.create 10
let tbl_PACKAGE_TYPE_CSTRS = MyHashtbl.create 10
let tbl_ROW_FIELD_LIST = MyHashtbl.create 10
let tbl_ROW_FIELD = MyHashtbl.create 10
let tbl_TAG_FIELD = MyHashtbl.create 10
let tbl_OPT_AMPERSAND = MyHashtbl.create 10
let tbl_AMPER_TYPE_LIST = MyHashtbl.create 10
let tbl_NAME_TAG_LIST = MyHashtbl.create 10
let tbl_SIMPLE_CORE_TYPE_OR_TUPLE = MyHashtbl.create 10
let tbl_SIMPLE_CORE_TYPE_OR_TUPLE_NO_ATTR = MyHashtbl.create 10
let tbl_CORE_TYPE_COMMA_LIST = MyHashtbl.create 10
let tbl_SIMPLE_CORE_TYPE = MyHashtbl.create 10
let tbl_CORE_TYPE_LIST_NO_ATTR = MyHashtbl.create 10
let tbl_CORE_TYPE_LIST = MyHashtbl.create 10
let tbl_SIMPLE_CORE_TYPE_NO_ATTR = MyHashtbl.create 10
let tbl_METH_LIST = MyHashtbl.create 10
let tbl_FIELD = MyHashtbl.create 10
let tbl_POLY_TYPE = MyHashtbl.create 10
let tbl_LABEL = MyHashtbl.create 10
let tbl_CONSTANT = MyHashtbl.create 10
let tbl_SIGNED_CONSTANT = MyHashtbl.create 10
let tbl_OPERATOR = MyHashtbl.create 10
let tbl_CONSTR_IDENT = MyHashtbl.create 10
let tbl_VAL_IDENT = MyHashtbl.create 10
let tbl_CONSTR_LONGIDENT = MyHashtbl.create 10
let tbl_LABEL_LONGIDENT = MyHashtbl.create 10
let tbl_TYPE_LONGIDENT = MyHashtbl.create 10
let tbl_MTY_LONGIDENT = MyHashtbl.create 10
let tbl_CLTY_LONGIDENT = MyHashtbl.create 10
let tbl_MOD_EXT_LONGIDENT = MyHashtbl.create 10
let tbl_CLASS_LONGIDENT = MyHashtbl.create 10
let tbl_MOD_LONGIDENT = MyHashtbl.create 10
let tbl_VAL_LONGIDENT = MyHashtbl.create 10
let tbl_TOPLEVEL_DIRECTIVE = MyHashtbl.create 10
let tbl_NAME_TAG = MyHashtbl.create 10
let tbl_IDENT = MyHashtbl.create 10
let tbl_REC_FLAG = MyHashtbl.create 10
let tbl_DIRECTION_FLAG = MyHashtbl.create 10
let tbl_PRIVATE_FLAG = MyHashtbl.create 10
let tbl_MUTABLE_FLAG = MyHashtbl.create 10
let tbl_VIRTUAL_FLAG = MyHashtbl.create 10
let tbl_PRIVATE_VIRTUAL_FLAGS = MyHashtbl.create 10
let tbl_OVERRIDE_FLAG = MyHashtbl.create 10
let tbl_OPT_BAR = MyHashtbl.create 10
let tbl_OPT_SEMI = MyHashtbl.create 10
let tbl_SUBTRACTIVE = MyHashtbl.create 10
let tbl_ADDITIVE = MyHashtbl.create 10
let tbl_SINGLE_ATTR_ID = MyHashtbl.create 10
let tbl_POST_ITEM_ATTRIBUTE = MyHashtbl.create 10
let tbl_POST_ITEM_ATTRIBUTES = MyHashtbl.create 10
let tbl_ATTRIBUTE = MyHashtbl.create 10
let tbl_EXT_ATTRIBUTES = MyHashtbl.create 10
let tbl_ATTRIBUTES = MyHashtbl.create 10
let tbl_EXTENSION = MyHashtbl.create 10
let tbl_ITEM_EXTENSION = MyHashtbl.create 10
let tbl_ATTR_ID = MyHashtbl.create 10
let tbl_STRUCTURE = MyHashtbl.create 10
let tbl_CORE_TYPE = MyHashtbl.create 10
let tbl_PAYLOAD = MyHashtbl.create 10
let tbl_PATTERN = MyHashtbl.create 10
let tbl_SEQ_EXPR = MyHashtbl.create 10
let tbl_reset _ = (
  let _ = MyHashtbl.clear tbl_IMPLEMENTATION in 
  let _ = MyHashtbl.clear tbl_INTERFACE in 
  let _ = MyHashtbl.clear tbl_TOPLEVEL_PHRASE in 
  let _ = MyHashtbl.clear tbl_TOP_STRUCTURE in 
  let _ = MyHashtbl.clear tbl_TOP_STRUCTURE_TAIL in 
  let _ = MyHashtbl.clear tbl_USE_FILE in 
  let _ = MyHashtbl.clear tbl_USE_FILE_TAIL in 
  let _ = MyHashtbl.clear tbl_PARSE_CORE_TYPE in 
  let _ = MyHashtbl.clear tbl_PARSE_EXPRESSION in 
  let _ = MyHashtbl.clear tbl_PARSE_PATTERN in 
  let _ = MyHashtbl.clear tbl_STRUCTURE_TAIL in 
  let _ = MyHashtbl.clear tbl_STR_ATTRIBUTE in 
  let _ = MyHashtbl.clear tbl_STRUCTURE_ITEM in 
  let _ = MyHashtbl.clear tbl_MODULE_BINDINGS in 
  let _ = MyHashtbl.clear tbl_MODULE_BINDING in 
  let _ = MyHashtbl.clear tbl_SIGNATURE in 
  let _ = MyHashtbl.clear tbl_SIGNATURE_TAIL in 
  let _ = MyHashtbl.clear tbl_SIG_ATTRIBUTE in 
  let _ = MyHashtbl.clear tbl_SIGNATURE_ITEM in 
  let _ = MyHashtbl.clear tbl_MODULE_DECLARATION in 
  let _ = MyHashtbl.clear tbl_MODULE_REC_DECLARATIONS in 
  let _ = MyHashtbl.clear tbl_MODULE_REC_DECLARATION in 
  let _ = MyHashtbl.clear tbl_MODULE_TYPE in 
  let _ = MyHashtbl.clear tbl_CLASS_DECLARATIONS in 
  let _ = MyHashtbl.clear tbl_CLASS_DECLARATION in 
  let _ = MyHashtbl.clear tbl_CLASS_FUN_BINDING in 
  let _ = MyHashtbl.clear tbl_CLASS_FUN_DEF in 
  let _ = MyHashtbl.clear tbl_CLASS_SIMPLE_EXPR in 
  let _ = MyHashtbl.clear tbl_CLASS_SELF_PATTERN in 
  let _ = MyHashtbl.clear tbl_CLASS_FIELDS in 
  let _ = MyHashtbl.clear tbl_CLASS_EXPR in 
  let _ = MyHashtbl.clear tbl_CLASS_FIELD in 
  let _ = MyHashtbl.clear tbl_PARENT_BINDER in 
  let _ = MyHashtbl.clear tbl_VALUE in 
  let _ = MyHashtbl.clear tbl_METHOD_ in 
  let _ = MyHashtbl.clear tbl_CLASS_SIG_BODY in 
  let _ = MyHashtbl.clear tbl_CLASS_SELF_TYPE in 
  let _ = MyHashtbl.clear tbl_CLASS_SIG_FIELDS in 
  let _ = MyHashtbl.clear tbl_CLASS_SIG_FIELD in 
  let _ = MyHashtbl.clear tbl_VALUE_TYPE in 
  let _ = MyHashtbl.clear tbl_CONSTRAIN_FIELD in 
  let _ = MyHashtbl.clear tbl_CLASS_DESCRIPTIONS in 
  let _ = MyHashtbl.clear tbl_CLASS_DESCRIPTION in 
  let _ = MyHashtbl.clear tbl_CLASS_TYPE in 
  let _ = MyHashtbl.clear tbl_CLASS_TYPE_DECLARATIONS in 
  let _ = MyHashtbl.clear tbl_CLASS_TYPE_DECLARATION in 
  let _ = MyHashtbl.clear tbl_CLASS_TYPE_PARAMETERS in 
  let _ = MyHashtbl.clear tbl_CLASS_SIGNATURE in 
  let _ = MyHashtbl.clear tbl_PATTERN_VAR in 
  let _ = MyHashtbl.clear tbl_OPT_DEFAULT in 
  let _ = MyHashtbl.clear tbl_LABEL_LET_PATTERN in 
  let _ = MyHashtbl.clear tbl_LABEL_VAR in 
  let _ = MyHashtbl.clear tbl_LET_PATTERN in 
  let _ = MyHashtbl.clear tbl_MODULE_BINDING_BODY in 
  let _ = MyHashtbl.clear tbl_CLASS_STRUCTURE in 
  let _ = MyHashtbl.clear tbl_MODULE_EXPR in 
  let _ = MyHashtbl.clear tbl_SIMPLE_LABELED_EXPR_LIST in 
  let _ = MyHashtbl.clear tbl_LABELED_SIMPLE_EXPR in 
  let _ = MyHashtbl.clear tbl_LABEL_EXPR in 
  let _ = MyHashtbl.clear tbl_LABEL_IDENT in 
  let _ = MyHashtbl.clear tbl_LET_BINDINGS in 
  let _ = MyHashtbl.clear tbl_LET_BINDING in 
  let _ = MyHashtbl.clear tbl_LIDENT_LIST in 
  let _ = MyHashtbl.clear tbl_LET_BINDING_ in 
  let _ = MyHashtbl.clear tbl_STRICT_BINDING in 
  let _ = MyHashtbl.clear tbl_FUN_BINDING in 
  let _ = MyHashtbl.clear tbl_MATCH_CASES in 
  let _ = MyHashtbl.clear tbl_MATCH_CASE in 
  let _ = MyHashtbl.clear tbl_LABELED_SIMPLE_PATTERN in 
  let _ = MyHashtbl.clear tbl_FUN_DEF in 
  let _ = MyHashtbl.clear tbl_EXPR_COMMA_LIST in 
  let _ = MyHashtbl.clear tbl_SIMPLE_EXPR in 
  let _ = MyHashtbl.clear tbl_RECORD_EXPR in 
  let _ = MyHashtbl.clear tbl_LBL_EXPR_LIST in 
  let _ = MyHashtbl.clear tbl_LBL_EXPR in 
  let _ = MyHashtbl.clear tbl_FIELD_EXPR_LIST in 
  let _ = MyHashtbl.clear tbl_EXPR_SEMI_LIST in 
  let _ = MyHashtbl.clear tbl_EXPR in 
  let _ = MyHashtbl.clear tbl_TYPE_CONSTRAINT in 
  let _ = MyHashtbl.clear tbl_SIMPLE_PATTERN in 
  let _ = MyHashtbl.clear tbl_SIMPLE_PATTERN_NOT_IDENT in 
  let _ = MyHashtbl.clear tbl_PATTERN_COMMA_LIST in 
  let _ = MyHashtbl.clear tbl_PATTERN_SEMI_LIST in 
  let _ = MyHashtbl.clear tbl_LBL_PATTERN_LIST in 
  let _ = MyHashtbl.clear tbl_LBL_PATTERN in 
  let _ = MyHashtbl.clear tbl_PRIMITIVE_DECLARATION in 
  let _ = MyHashtbl.clear tbl_TYPE_DECLARATIONS in 
  let _ = MyHashtbl.clear tbl_TYPE_DECLARATION in 
  let _ = MyHashtbl.clear tbl_CONSTRAIN in 
  let _ = MyHashtbl.clear tbl_TYPE_KIND in 
  let _ = MyHashtbl.clear tbl_OPTIONAL_TYPE_PARAMETERS in 
  let _ = MyHashtbl.clear tbl_OPTIONAL_TYPE_PARAMETER_LIST in 
  let _ = MyHashtbl.clear tbl_OPTIONAL_TYPE_PARAMETER in 
  let _ = MyHashtbl.clear tbl_TYPE_VARIANCE in 
  let _ = MyHashtbl.clear tbl_TYPE_PARAMETER_LIST in 
  let _ = MyHashtbl.clear tbl_TYPE_PARAMETER in 
  let _ = MyHashtbl.clear tbl_CONSTRUCTOR_DECLARATIONS in 
  let _ = MyHashtbl.clear tbl_EXCEPTION_DECLARATION in 
  let _ = MyHashtbl.clear tbl_CONSTRUCTOR_DECLARATION in 
  let _ = MyHashtbl.clear tbl_GENERALIZED_CONSTRUCTOR_ARGUMENTS in 
  let _ = MyHashtbl.clear tbl_LABEL_DECLARATIONS in 
  let _ = MyHashtbl.clear tbl_LABEL_DECLARATION in 
  let _ = MyHashtbl.clear tbl_WITH_CONSTRAINTS in 
  let _ = MyHashtbl.clear tbl_CONSTRAINTS in 
  let _ = MyHashtbl.clear tbl_TYPE_PARAMETERS in 
  let _ = MyHashtbl.clear tbl_WITH_CONSTRAINT in 
  let _ = MyHashtbl.clear tbl_WITH_TYPE_BINDER in 
  let _ = MyHashtbl.clear tbl_TYPEVAR_LIST in 
  let _ = MyHashtbl.clear tbl_CORE_TYPE2 in 
  let _ = MyHashtbl.clear tbl_SIMPLE_CORE_TYPE2 in 
  let _ = MyHashtbl.clear tbl_PACKAGE_TYPE in 
  let _ = MyHashtbl.clear tbl_PACKAGE_TYPE_CSTR in 
  let _ = MyHashtbl.clear tbl_PACKAGE_TYPE_CSTRS in 
  let _ = MyHashtbl.clear tbl_ROW_FIELD_LIST in 
  let _ = MyHashtbl.clear tbl_ROW_FIELD in 
  let _ = MyHashtbl.clear tbl_TAG_FIELD in 
  let _ = MyHashtbl.clear tbl_OPT_AMPERSAND in 
  let _ = MyHashtbl.clear tbl_AMPER_TYPE_LIST in 
  let _ = MyHashtbl.clear tbl_NAME_TAG_LIST in 
  let _ = MyHashtbl.clear tbl_SIMPLE_CORE_TYPE_OR_TUPLE in 
  let _ = MyHashtbl.clear tbl_SIMPLE_CORE_TYPE_OR_TUPLE_NO_ATTR in 
  let _ = MyHashtbl.clear tbl_CORE_TYPE_COMMA_LIST in 
  let _ = MyHashtbl.clear tbl_SIMPLE_CORE_TYPE in 
  let _ = MyHashtbl.clear tbl_CORE_TYPE_LIST_NO_ATTR in 
  let _ = MyHashtbl.clear tbl_CORE_TYPE_LIST in 
  let _ = MyHashtbl.clear tbl_SIMPLE_CORE_TYPE_NO_ATTR in 
  let _ = MyHashtbl.clear tbl_METH_LIST in 
  let _ = MyHashtbl.clear tbl_FIELD in 
  let _ = MyHashtbl.clear tbl_POLY_TYPE in 
  let _ = MyHashtbl.clear tbl_LABEL in 
  let _ = MyHashtbl.clear tbl_CONSTANT in 
  let _ = MyHashtbl.clear tbl_SIGNED_CONSTANT in 
  let _ = MyHashtbl.clear tbl_OPERATOR in 
  let _ = MyHashtbl.clear tbl_CONSTR_IDENT in 
  let _ = MyHashtbl.clear tbl_VAL_IDENT in 
  let _ = MyHashtbl.clear tbl_CONSTR_LONGIDENT in 
  let _ = MyHashtbl.clear tbl_LABEL_LONGIDENT in 
  let _ = MyHashtbl.clear tbl_TYPE_LONGIDENT in 
  let _ = MyHashtbl.clear tbl_MTY_LONGIDENT in 
  let _ = MyHashtbl.clear tbl_CLTY_LONGIDENT in 
  let _ = MyHashtbl.clear tbl_MOD_EXT_LONGIDENT in 
  let _ = MyHashtbl.clear tbl_CLASS_LONGIDENT in 
  let _ = MyHashtbl.clear tbl_MOD_LONGIDENT in 
  let _ = MyHashtbl.clear tbl_VAL_LONGIDENT in 
  let _ = MyHashtbl.clear tbl_TOPLEVEL_DIRECTIVE in 
  let _ = MyHashtbl.clear tbl_NAME_TAG in 
  let _ = MyHashtbl.clear tbl_IDENT in 
  let _ = MyHashtbl.clear tbl_REC_FLAG in 
  let _ = MyHashtbl.clear tbl_DIRECTION_FLAG in 
  let _ = MyHashtbl.clear tbl_PRIVATE_FLAG in 
  let _ = MyHashtbl.clear tbl_MUTABLE_FLAG in 
  let _ = MyHashtbl.clear tbl_VIRTUAL_FLAG in 
  let _ = MyHashtbl.clear tbl_PRIVATE_VIRTUAL_FLAGS in 
  let _ = MyHashtbl.clear tbl_OVERRIDE_FLAG in 
  let _ = MyHashtbl.clear tbl_OPT_BAR in 
  let _ = MyHashtbl.clear tbl_OPT_SEMI in 
  let _ = MyHashtbl.clear tbl_SUBTRACTIVE in 
  let _ = MyHashtbl.clear tbl_ADDITIVE in 
  let _ = MyHashtbl.clear tbl_SINGLE_ATTR_ID in 
  let _ = MyHashtbl.clear tbl_POST_ITEM_ATTRIBUTE in 
  let _ = MyHashtbl.clear tbl_POST_ITEM_ATTRIBUTES in 
  let _ = MyHashtbl.clear tbl_ATTRIBUTE in 
  let _ = MyHashtbl.clear tbl_EXT_ATTRIBUTES in 
  let _ = MyHashtbl.clear tbl_ATTRIBUTES in 
  let _ = MyHashtbl.clear tbl_EXTENSION in 
  let _ = MyHashtbl.clear tbl_ITEM_EXTENSION in 
  let _ = MyHashtbl.clear tbl_ATTR_ID in 
  let _ = MyHashtbl.clear tbl_STRUCTURE in 
  let _ = MyHashtbl.clear tbl_CORE_TYPE in 
  let _ = MyHashtbl.clear tbl_PAYLOAD in 
  let _ = MyHashtbl.clear tbl_PATTERN in 
  let _ = MyHashtbl.clear tbl_SEQ_EXPR in 
())

let rec parse_IMPLEMENTATION = fun i -> (mcu4 tbl_IMPLEMENTATION "IMPLEMENTATION" (fun i -> (*unique4*) ((((parse_STRUCTURE***>(term_to_parser "?EOF?")) >>>> ( fIXME ))) i)) i)

 and parse_INTERFACE = fun i -> (mcu4 tbl_INTERFACE "INTERFACE" (fun i -> (*unique4*) ((((parse_SIGNATURE***>(term_to_parser "?EOF?")) >>>> ( fIXME ))) i)) i)

 and parse_TOPLEVEL_PHRASE = fun i -> (mcu4 tbl_TOPLEVEL_PHRASE "TOPLEVEL_PHRASE" (fun i -> (*unique4*) ((((parse_TOP_STRUCTURE***>(term_to_parser "?SEMISEMI?")) >>>> ( fIXME ))||||((parse_TOPLEVEL_DIRECTIVE***>(term_to_parser "?SEMISEMI?")) >>>> ( fIXME ))||||(((term_to_parser "?EOF?")) >>>> ( fIXME ))) i)) i)

 and parse_TOP_STRUCTURE = fun i -> (mcu4 tbl_TOP_STRUCTURE "TOP_STRUCTURE" (fun i -> (*unique4*) ((((parse_STR_ATTRIBUTE***>parse_TOP_STRUCTURE) >>>> ( fIXME ))||||((parse_SEQ_EXPR***>parse_POST_ITEM_ATTRIBUTES) >>>> ( fIXME ))||||((parse_TOP_STRUCTURE_TAIL) >>>> ( fIXME ))) i)) i)

 and parse_TOP_STRUCTURE_TAIL = fun i -> (mcu4 tbl_TOP_STRUCTURE_TAIL "TOP_STRUCTURE_TAIL" (fun i -> (*unique4*) (((((term_to_parser "?eps?")) >>>> ( fIXME ))||||((parse_STRUCTURE_ITEM***>parse_TOP_STRUCTURE_TAIL) >>>> ( fIXME ))) i)) i)

 and parse_USE_FILE = fun i -> (mcu4 tbl_USE_FILE "USE_FILE" (fun i -> (*unique4*) ((((parse_USE_FILE_TAIL) >>>> ( fIXME ))||||((parse_SEQ_EXPR***>parse_POST_ITEM_ATTRIBUTES***>parse_USE_FILE_TAIL) >>>> ( fIXME ))) i)) i)

 and parse_USE_FILE_TAIL = fun i -> (mcu4 tbl_USE_FILE_TAIL "USE_FILE_TAIL" (fun i -> (*unique4*) (((((term_to_parser "?EOF?")) >>>> ( fIXME ))||||(((term_to_parser "?SEMISEMI?")***>(term_to_parser "?EOF?")) >>>> ( fIXME ))||||(((term_to_parser "?SEMISEMI?")***>parse_SEQ_EXPR***>parse_POST_ITEM_ATTRIBUTES***>parse_USE_FILE_TAIL) >>>> ( fIXME ))||||(((term_to_parser "?SEMISEMI?")***>parse_STRUCTURE_ITEM***>parse_USE_FILE_TAIL) >>>> ( fIXME ))||||(((term_to_parser "?SEMISEMI?")***>parse_TOPLEVEL_DIRECTIVE***>parse_USE_FILE_TAIL) >>>> ( fIXME ))||||((parse_STRUCTURE_ITEM***>parse_USE_FILE_TAIL) >>>> ( fIXME ))||||((parse_TOPLEVEL_DIRECTIVE***>parse_USE_FILE_TAIL) >>>> ( fIXME ))) i)) i)

 and parse_PARSE_CORE_TYPE = fun i -> (mcu4 tbl_PARSE_CORE_TYPE "PARSE_CORE_TYPE" (fun i -> (*unique4*) ((((parse_CORE_TYPE***>(term_to_parser "?EOF?")) >>>> ( fIXME ))) i)) i)

 and parse_PARSE_EXPRESSION = fun i -> (mcu4 tbl_PARSE_EXPRESSION "PARSE_EXPRESSION" (fun i -> (*unique4*) ((((parse_SEQ_EXPR***>(term_to_parser "?EOF?")) >>>> ( fIXME ))) i)) i)

 and parse_PARSE_PATTERN = fun i -> (mcu4 tbl_PARSE_PATTERN "PARSE_PATTERN" (fun i -> (*unique4*) ((((parse_PATTERN***>(term_to_parser "?EOF?")) >>>> ( fIXME ))) i)) i)

 and parse_STRUCTURE_TAIL = fun i -> (mcu4 tbl_STRUCTURE_TAIL "STRUCTURE_TAIL" (fun i -> (*unique4*) (((((term_to_parser "?eps?")) >>>> ( fIXME ))||||(((term_to_parser "?SEMISEMI?")***>parse_STRUCTURE) >>>> ( fIXME ))||||((parse_STRUCTURE_ITEM***>parse_STRUCTURE_TAIL) >>>> ( fIXME ))) i)) i)

 and parse_STR_ATTRIBUTE = fun i -> (mcu4 tbl_STR_ATTRIBUTE "STR_ATTRIBUTE" (fun i -> (*unique4*) ((((parse_POST_ITEM_ATTRIBUTE) >>>> ( fIXME ))) i)) i)

 and parse_STRUCTURE_ITEM = fun i -> (mcu4 tbl_STRUCTURE_ITEM "STRUCTURE_ITEM" (fun i -> (*unique4*) (((((term_to_parser "?LET?")***>parse_EXT_ATTRIBUTES***>parse_REC_FLAG***>parse_LET_BINDINGS) >>>> ( fIXME ))||||(((term_to_parser "?EXTERNAL?")***>parse_VAL_IDENT***>(term_to_parser "?COLON?")***>parse_CORE_TYPE***>(term_to_parser "?EQUAL?")***>parse_PRIMITIVE_DECLARATION***>parse_POST_ITEM_ATTRIBUTES) >>>> ( fIXME ))||||(((term_to_parser "?TYPE?")***>parse_TYPE_DECLARATIONS) >>>> ( fIXME ))||||(((term_to_parser "?EXCEPTION?")***>parse_EXCEPTION_DECLARATION) >>>> ( fIXME ))||||(((term_to_parser "?EXCEPTION?")***>(term_to_parser "?UIDENT?")***>(term_to_parser "?EQUAL?")***>parse_CONSTR_LONGIDENT***>parse_POST_ITEM_ATTRIBUTES) >>>> ( fIXME ))||||(((term_to_parser "?MODULE?")***>parse_MODULE_BINDING) >>>> ( fIXME ))||||(((term_to_parser "?MODULE?")***>(term_to_parser "?REC?")***>parse_MODULE_BINDINGS) >>>> ( fIXME ))||||(((term_to_parser "?MODULE?")***>(term_to_parser "?TYPE?")***>parse_IDENT***>parse_POST_ITEM_ATTRIBUTES) >>>> ( fIXME ))||||(((term_to_parser "?MODULE?")***>(term_to_parser "?TYPE?")***>parse_IDENT***>(term_to_parser "?EQUAL?")***>parse_MODULE_TYPE***>parse_POST_ITEM_ATTRIBUTES) >>>> ( fIXME ))||||(((term_to_parser "?OPEN?")***>parse_OVERRIDE_FLAG***>parse_MOD_LONGIDENT***>parse_POST_ITEM_ATTRIBUTES) >>>> ( fIXME ))||||(((term_to_parser "?CLASS?")***>parse_CLASS_DECLARATIONS) >>>> ( fIXME ))||||(((term_to_parser "?CLASS?")***>(term_to_parser "?TYPE?")***>parse_CLASS_TYPE_DECLARATIONS) >>>> ( fIXME ))||||(((term_to_parser "?INCLUDE?")***>parse_MODULE_EXPR***>parse_POST_ITEM_ATTRIBUTES) >>>> ( fIXME ))||||((parse_ITEM_EXTENSION***>parse_POST_ITEM_ATTRIBUTES) >>>> ( fIXME ))) i)) i)

 and parse_MODULE_BINDINGS = fun i -> (mcu4 tbl_MODULE_BINDINGS "MODULE_BINDINGS" (fun i -> (*unique4*) ((((parse_MODULE_BINDING) >>>> ( fIXME ))||||((parse_MODULE_BINDINGS***>(term_to_parser "?AND?")***>parse_MODULE_BINDING) >>>> ( fIXME ))) i)) i)

 and parse_MODULE_BINDING = fun i -> (mcu4 tbl_MODULE_BINDING "MODULE_BINDING" (fun i -> (*unique4*) (((((term_to_parser "?UIDENT?")***>parse_MODULE_BINDING_BODY***>parse_POST_ITEM_ATTRIBUTES) >>>> ( fIXME ))) i)) i)

 and parse_SIGNATURE = fun i -> (mcu4 tbl_SIGNATURE "SIGNATURE" (fun i -> (*unique4*) ((((parse_SIG_ATTRIBUTE***>parse_SIGNATURE) >>>> ( fIXME ))||||((parse_SIGNATURE_TAIL) >>>> ( fIXME ))) i)) i)

 and parse_SIGNATURE_TAIL = fun i -> (mcu4 tbl_SIGNATURE_TAIL "SIGNATURE_TAIL" (fun i -> (*unique4*) (((((term_to_parser "?eps?")) >>>> ( fIXME ))||||(((term_to_parser "?SEMISEMI?")***>parse_SIGNATURE) >>>> ( fIXME ))||||((parse_SIGNATURE_ITEM***>parse_SIGNATURE_TAIL) >>>> ( fIXME ))) i)) i)

 and parse_SIG_ATTRIBUTE = fun i -> (mcu4 tbl_SIG_ATTRIBUTE "SIG_ATTRIBUTE" (fun i -> (*unique4*) ((((parse_POST_ITEM_ATTRIBUTE) >>>> ( fIXME ))) i)) i)

 and parse_SIGNATURE_ITEM = fun i -> (mcu4 tbl_SIGNATURE_ITEM "SIGNATURE_ITEM" (fun i -> (*unique4*) (((((term_to_parser "?VAL?")***>parse_VAL_IDENT***>(term_to_parser "?COLON?")***>parse_CORE_TYPE***>parse_POST_ITEM_ATTRIBUTES) >>>> ( fIXME ))||||(((term_to_parser "?EXTERNAL?")***>parse_VAL_IDENT***>(term_to_parser "?COLON?")***>parse_CORE_TYPE***>(term_to_parser "?EQUAL?")***>parse_PRIMITIVE_DECLARATION***>parse_POST_ITEM_ATTRIBUTES) >>>> ( fIXME ))||||(((term_to_parser "?TYPE?")***>parse_TYPE_DECLARATIONS) >>>> ( fIXME ))||||(((term_to_parser "?EXCEPTION?")***>parse_EXCEPTION_DECLARATION) >>>> ( fIXME ))||||(((term_to_parser "?MODULE?")***>(term_to_parser "?UIDENT?")***>parse_MODULE_DECLARATION***>parse_POST_ITEM_ATTRIBUTES) >>>> ( fIXME ))||||(((term_to_parser "?MODULE?")***>(term_to_parser "?REC?")***>parse_MODULE_REC_DECLARATIONS) >>>> ( fIXME ))||||(((term_to_parser "?MODULE?")***>(term_to_parser "?TYPE?")***>parse_IDENT***>parse_POST_ITEM_ATTRIBUTES) >>>> ( fIXME ))||||(((term_to_parser "?MODULE?")***>(term_to_parser "?TYPE?")***>parse_IDENT***>(term_to_parser "?EQUAL?")***>parse_MODULE_TYPE***>parse_POST_ITEM_ATTRIBUTES) >>>> ( fIXME ))||||(((term_to_parser "?OPEN?")***>parse_OVERRIDE_FLAG***>parse_MOD_LONGIDENT***>parse_POST_ITEM_ATTRIBUTES) >>>> ( fIXME ))||||(((term_to_parser "?INCLUDE?")***>parse_MODULE_TYPE***>parse_POST_ITEM_ATTRIBUTES) >>>> ( fIXME ))||||(((term_to_parser "?CLASS?")***>parse_CLASS_DESCRIPTIONS) >>>> ( fIXME ))||||(((term_to_parser "?CLASS?")***>(term_to_parser "?TYPE?")***>parse_CLASS_TYPE_DECLARATIONS) >>>> ( fIXME ))||||((parse_ITEM_EXTENSION***>parse_POST_ITEM_ATTRIBUTES) >>>> ( fIXME ))) i)) i)

 and parse_MODULE_DECLARATION = fun i -> (mcu4 tbl_MODULE_DECLARATION "MODULE_DECLARATION" (fun i -> (*unique4*) (((((term_to_parser "?COLON?")***>parse_MODULE_TYPE) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>(term_to_parser "?UIDENT?")***>(term_to_parser "?COLON?")***>parse_MODULE_TYPE***>(term_to_parser "?RPAREN?")***>parse_MODULE_DECLARATION) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>(term_to_parser "?RPAREN?")***>parse_MODULE_DECLARATION) >>>> ( fIXME ))) i)) i)

 and parse_MODULE_REC_DECLARATIONS = fun i -> (mcu4 tbl_MODULE_REC_DECLARATIONS "MODULE_REC_DECLARATIONS" (fun i -> (*unique4*) ((((parse_MODULE_REC_DECLARATION) >>>> ( fIXME ))||||((parse_MODULE_REC_DECLARATIONS***>(term_to_parser "?AND?")***>parse_MODULE_REC_DECLARATION) >>>> ( fIXME ))) i)) i)

 and parse_MODULE_REC_DECLARATION = fun i -> (mcu4 tbl_MODULE_REC_DECLARATION "MODULE_REC_DECLARATION" (fun i -> (*unique4*) (((((term_to_parser "?UIDENT?")***>(term_to_parser "?COLON?")***>parse_MODULE_TYPE***>parse_POST_ITEM_ATTRIBUTES) >>>> ( fIXME ))) i)) i)

 and parse_MODULE_TYPE = fun i -> (mcu4 tbl_MODULE_TYPE "MODULE_TYPE" (fun i -> (*unique4*) ((((parse_MTY_LONGIDENT) >>>> ( fIXME ))||||(((term_to_parser "?SIG?")***>parse_SIGNATURE***>(term_to_parser "?END?")) >>>> ( fIXME ))||||(((term_to_parser "?FUNCTOR?")***>(term_to_parser "?LPAREN?")***>(term_to_parser "?UIDENT?")***>(term_to_parser "?COLON?")***>parse_MODULE_TYPE***>(term_to_parser "?RPAREN?")***>(term_to_parser "?MINUSGREATER?")***>parse_MODULE_TYPE) >>>> ( fIXME ))||||(((term_to_parser "?FUNCTOR?")***>(term_to_parser "?LPAREN?")***>(term_to_parser "?RPAREN?")***>(term_to_parser "?MINUSGREATER?")***>parse_MODULE_TYPE) >>>> ( fIXME ))||||((parse_MODULE_TYPE***>(term_to_parser "?WITH?")***>parse_WITH_CONSTRAINTS) >>>> ( fIXME ))||||(((term_to_parser "?MODULE?")***>(term_to_parser "?TYPE?")***>(term_to_parser "?OF?")***>parse_MODULE_EXPR) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>parse_MODULE_TYPE***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||((parse_EXTENSION) >>>> ( fIXME ))||||((parse_MODULE_TYPE***>parse_ATTRIBUTE) >>>> ( fIXME ))) i)) i)

 and parse_CLASS_DECLARATIONS = fun i -> (mcu4 tbl_CLASS_DECLARATIONS "CLASS_DECLARATIONS" (fun i -> (*unique4*) ((((parse_CLASS_DECLARATIONS***>(term_to_parser "?AND?")***>parse_CLASS_DECLARATION) >>>> ( fIXME ))||||((parse_CLASS_DECLARATION) >>>> ( fIXME ))) i)) i)

 and parse_CLASS_DECLARATION = fun i -> (mcu4 tbl_CLASS_DECLARATION "CLASS_DECLARATION" (fun i -> (*unique4*) ((((parse_VIRTUAL_FLAG***>parse_CLASS_TYPE_PARAMETERS***>(term_to_parser "?LIDENT?")***>parse_CLASS_FUN_BINDING***>parse_POST_ITEM_ATTRIBUTES) >>>> ( fIXME ))) i)) i)

 and parse_CLASS_FUN_BINDING = fun i -> (mcu4 tbl_CLASS_FUN_BINDING "CLASS_FUN_BINDING" (fun i -> (*unique4*) (((((term_to_parser "?EQUAL?")***>parse_CLASS_EXPR) >>>> ( fIXME ))||||(((term_to_parser "?COLON?")***>parse_CLASS_TYPE***>(term_to_parser "?EQUAL?")***>parse_CLASS_EXPR) >>>> ( fIXME ))||||((parse_LABELED_SIMPLE_PATTERN***>parse_CLASS_FUN_BINDING) >>>> ( fIXME ))) i)) i)

 and parse_CLASS_FUN_DEF = fun i -> (mcu4 tbl_CLASS_FUN_DEF "CLASS_FUN_DEF" (fun i -> (*unique4*) ((((parse_LABELED_SIMPLE_PATTERN***>(term_to_parser "?MINUSGREATER?")***>parse_CLASS_EXPR) >>>> ( fIXME ))||||((parse_LABELED_SIMPLE_PATTERN***>parse_CLASS_FUN_DEF) >>>> ( fIXME ))) i)) i)

 and parse_CLASS_SIMPLE_EXPR = fun i -> (mcu4 tbl_CLASS_SIMPLE_EXPR "CLASS_SIMPLE_EXPR" (fun i -> (*unique4*) (((((term_to_parser "?LBRACKET?")***>parse_CORE_TYPE_COMMA_LIST***>(term_to_parser "?RBRACKET?")***>parse_CLASS_LONGIDENT) >>>> ( fIXME ))||||((parse_CLASS_LONGIDENT) >>>> ( fIXME ))||||(((term_to_parser "?OBJECT?")***>parse_CLASS_STRUCTURE***>(term_to_parser "?END?")) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>parse_CLASS_EXPR***>(term_to_parser "?COLON?")***>parse_CLASS_TYPE***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>parse_CLASS_EXPR***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))) i)) i)

 and parse_CLASS_SELF_PATTERN = fun i -> (mcu4 tbl_CLASS_SELF_PATTERN "CLASS_SELF_PATTERN" (fun i -> (*unique4*) (((((term_to_parser "?LPAREN?")***>parse_PATTERN***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>parse_PATTERN***>(term_to_parser "?COLON?")***>parse_CORE_TYPE***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||(((term_to_parser "?eps?")) >>>> ( fIXME ))) i)) i)

 and parse_CLASS_FIELDS = fun i -> (mcu4 tbl_CLASS_FIELDS "CLASS_FIELDS" (fun i -> (*unique4*) (((((term_to_parser "?eps?")) >>>> ( fIXME ))||||((parse_CLASS_FIELDS***>parse_CLASS_FIELD) >>>> ( fIXME ))) i)) i)

 and parse_CLASS_EXPR = fun i -> (mcu4 tbl_CLASS_EXPR "CLASS_EXPR" (fun i -> (*unique4*) ((((parse_CLASS_SIMPLE_EXPR) >>>> ( fIXME ))||||(((term_to_parser "?FUN?")***>parse_CLASS_FUN_DEF) >>>> ( fIXME ))||||((parse_CLASS_SIMPLE_EXPR***>parse_SIMPLE_LABELED_EXPR_LIST) >>>> ( fIXME ))||||(((term_to_parser "?LET?")***>parse_REC_FLAG***>parse_LET_BINDINGS***>(term_to_parser "?IN?")***>parse_CLASS_EXPR) >>>> ( fIXME ))||||((parse_CLASS_EXPR***>parse_ATTRIBUTE) >>>> ( fIXME ))||||((parse_EXTENSION) >>>> ( fIXME ))) i)) i)

 and parse_CLASS_FIELD = fun i -> (mcu4 tbl_CLASS_FIELD "CLASS_FIELD" (fun i -> (*unique4*) (((((term_to_parser "?INHERIT?")***>parse_OVERRIDE_FLAG***>parse_CLASS_EXPR***>parse_PARENT_BINDER) >>>> ( fIXME ))||||(((term_to_parser "?VAL?")***>parse_VALUE) >>>> ( fIXME ))||||(((term_to_parser "?METHOD?")***>parse_METHOD_) >>>> ( fIXME ))||||(((term_to_parser "?CONSTRAINT?")***>parse_CONSTRAIN_FIELD) >>>> ( fIXME ))||||(((term_to_parser "?INITIALIZER?")***>parse_SEQ_EXPR) >>>> ( fIXME ))||||((parse_CLASS_FIELD***>parse_POST_ITEM_ATTRIBUTE) >>>> ( fIXME ))||||((parse_ITEM_EXTENSION) >>>> ( fIXME ))) i)) i)

 and parse_PARENT_BINDER = fun i -> (mcu4 tbl_PARENT_BINDER "PARENT_BINDER" (fun i -> (*unique4*) (((((term_to_parser "?AS?")***>(term_to_parser "?LIDENT?")) >>>> ( fIXME ))||||(((term_to_parser "?eps?")) >>>> ( fIXME ))) i)) i)

 and parse_VALUE = fun i -> (mcu4 tbl_VALUE "VALUE" (fun i -> (*unique4*) ((((parse_OVERRIDE_FLAG***>(term_to_parser "?MUTABLE?")***>(term_to_parser "?VIRTUAL?")***>parse_LABEL***>(term_to_parser "?COLON?")***>parse_CORE_TYPE) >>>> ( fIXME ))||||(((term_to_parser "?VIRTUAL?")***>parse_MUTABLE_FLAG***>parse_LABEL***>(term_to_parser "?COLON?")***>parse_CORE_TYPE) >>>> ( fIXME ))||||((parse_OVERRIDE_FLAG***>parse_MUTABLE_FLAG***>parse_LABEL***>(term_to_parser "?EQUAL?")***>parse_SEQ_EXPR) >>>> ( fIXME ))||||((parse_OVERRIDE_FLAG***>parse_MUTABLE_FLAG***>parse_LABEL***>parse_TYPE_CONSTRAINT***>(term_to_parser "?EQUAL?")***>parse_SEQ_EXPR) >>>> ( fIXME ))) i)) i)

 and parse_METHOD_ = fun i -> (mcu4 tbl_METHOD_ "METHOD_" (fun i -> (*unique4*) ((((parse_OVERRIDE_FLAG***>(term_to_parser "?PRIVATE?")***>(term_to_parser "?VIRTUAL?")***>parse_LABEL***>(term_to_parser "?COLON?")***>parse_POLY_TYPE) >>>> ( fIXME ))||||((parse_OVERRIDE_FLAG***>(term_to_parser "?VIRTUAL?")***>parse_PRIVATE_FLAG***>parse_LABEL***>(term_to_parser "?COLON?")***>parse_POLY_TYPE) >>>> ( fIXME ))||||((parse_OVERRIDE_FLAG***>parse_PRIVATE_FLAG***>parse_LABEL***>parse_STRICT_BINDING) >>>> ( fIXME ))||||((parse_OVERRIDE_FLAG***>parse_PRIVATE_FLAG***>parse_LABEL***>(term_to_parser "?COLON?")***>parse_POLY_TYPE***>(term_to_parser "?EQUAL?")***>parse_SEQ_EXPR) >>>> ( fIXME ))||||((parse_OVERRIDE_FLAG***>parse_PRIVATE_FLAG***>parse_LABEL***>(term_to_parser "?COLON?")***>(term_to_parser "?TYPE?")***>parse_LIDENT_LIST***>(term_to_parser "?DOT?")***>parse_CORE_TYPE***>(term_to_parser "?EQUAL?")***>parse_SEQ_EXPR) >>>> ( fIXME ))) i)) i)

 and parse_CLASS_SIG_BODY = fun i -> (mcu4 tbl_CLASS_SIG_BODY "CLASS_SIG_BODY" (fun i -> (*unique4*) ((((parse_CLASS_SELF_TYPE***>parse_CLASS_SIG_FIELDS) >>>> ( fIXME ))) i)) i)

 and parse_CLASS_SELF_TYPE = fun i -> (mcu4 tbl_CLASS_SELF_TYPE "CLASS_SELF_TYPE" (fun i -> (*unique4*) (((((term_to_parser "?LPAREN?")***>parse_CORE_TYPE***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||(((term_to_parser "?eps?")) >>>> ( fIXME ))) i)) i)

 and parse_CLASS_SIG_FIELDS = fun i -> (mcu4 tbl_CLASS_SIG_FIELDS "CLASS_SIG_FIELDS" (fun i -> (*unique4*) (((((term_to_parser "?eps?")) >>>> ( fIXME ))||||((parse_CLASS_SIG_FIELDS***>parse_CLASS_SIG_FIELD) >>>> ( fIXME ))) i)) i)

 and parse_CLASS_SIG_FIELD = fun i -> (mcu4 tbl_CLASS_SIG_FIELD "CLASS_SIG_FIELD" (fun i -> (*unique4*) (((((term_to_parser "?INHERIT?")***>parse_CLASS_SIGNATURE) >>>> ( fIXME ))||||(((term_to_parser "?VAL?")***>parse_VALUE_TYPE) >>>> ( fIXME ))||||(((term_to_parser "?METHOD?")***>parse_PRIVATE_VIRTUAL_FLAGS***>parse_LABEL***>(term_to_parser "?COLON?")***>parse_POLY_TYPE) >>>> ( fIXME ))||||(((term_to_parser "?CONSTRAINT?")***>parse_CONSTRAIN_FIELD) >>>> ( fIXME ))||||((parse_CLASS_SIG_FIELD***>parse_POST_ITEM_ATTRIBUTE) >>>> ( fIXME ))||||((parse_ITEM_EXTENSION) >>>> ( fIXME ))) i)) i)

 and parse_VALUE_TYPE = fun i -> (mcu4 tbl_VALUE_TYPE "VALUE_TYPE" (fun i -> (*unique4*) (((((term_to_parser "?VIRTUAL?")***>parse_MUTABLE_FLAG***>parse_LABEL***>(term_to_parser "?COLON?")***>parse_CORE_TYPE) >>>> ( fIXME ))||||(((term_to_parser "?MUTABLE?")***>parse_VIRTUAL_FLAG***>parse_LABEL***>(term_to_parser "?COLON?")***>parse_CORE_TYPE) >>>> ( fIXME ))||||((parse_LABEL***>(term_to_parser "?COLON?")***>parse_CORE_TYPE) >>>> ( fIXME ))) i)) i)

 and parse_CONSTRAIN_FIELD = fun i -> (mcu4 tbl_CONSTRAIN_FIELD "CONSTRAIN_FIELD" (fun i -> (*unique4*) ((((parse_CORE_TYPE***>(term_to_parser "?EQUAL?")***>parse_CORE_TYPE) >>>> ( fIXME ))) i)) i)

 and parse_CLASS_DESCRIPTIONS = fun i -> (mcu4 tbl_CLASS_DESCRIPTIONS "CLASS_DESCRIPTIONS" (fun i -> (*unique4*) ((((parse_CLASS_DESCRIPTIONS***>(term_to_parser "?AND?")***>parse_CLASS_DESCRIPTION) >>>> ( fIXME ))||||((parse_CLASS_DESCRIPTION) >>>> ( fIXME ))) i)) i)

 and parse_CLASS_DESCRIPTION = fun i -> (mcu4 tbl_CLASS_DESCRIPTION "CLASS_DESCRIPTION" (fun i -> (*unique4*) ((((parse_VIRTUAL_FLAG***>parse_CLASS_TYPE_PARAMETERS***>(term_to_parser "?LIDENT?")***>(term_to_parser "?COLON?")***>parse_CLASS_TYPE***>parse_POST_ITEM_ATTRIBUTES) >>>> ( fIXME ))) i)) i)

 and parse_CLASS_TYPE = fun i -> (mcu4 tbl_CLASS_TYPE "CLASS_TYPE" (fun i -> (*unique4*) ((((parse_CLASS_SIGNATURE) >>>> ( fIXME ))||||(((term_to_parser "?QUESTION?")***>(term_to_parser "?LIDENT?")***>(term_to_parser "?COLON?")***>parse_SIMPLE_CORE_TYPE_OR_TUPLE_NO_ATTR***>(term_to_parser "?MINUSGREATER?")***>parse_CLASS_TYPE) >>>> ( fIXME ))||||(((term_to_parser "?OPTLABEL?")***>parse_SIMPLE_CORE_TYPE_OR_TUPLE_NO_ATTR***>(term_to_parser "?MINUSGREATER?")***>parse_CLASS_TYPE) >>>> ( fIXME ))||||(((term_to_parser "?LIDENT?")***>(term_to_parser "?COLON?")***>parse_SIMPLE_CORE_TYPE_OR_TUPLE_NO_ATTR***>(term_to_parser "?MINUSGREATER?")***>parse_CLASS_TYPE) >>>> ( fIXME ))||||((parse_SIMPLE_CORE_TYPE_OR_TUPLE_NO_ATTR***>(term_to_parser "?MINUSGREATER?")***>parse_CLASS_TYPE) >>>> ( fIXME ))||||((parse_CLASS_TYPE***>parse_ATTRIBUTE) >>>> ( fIXME ))||||((parse_EXTENSION) >>>> ( fIXME ))) i)) i)

 and parse_CLASS_TYPE_DECLARATIONS = fun i -> (mcu4 tbl_CLASS_TYPE_DECLARATIONS "CLASS_TYPE_DECLARATIONS" (fun i -> (*unique4*) ((((parse_CLASS_TYPE_DECLARATIONS***>(term_to_parser "?AND?")***>parse_CLASS_TYPE_DECLARATION) >>>> ( fIXME ))||||((parse_CLASS_TYPE_DECLARATION) >>>> ( fIXME ))) i)) i)

 and parse_CLASS_TYPE_DECLARATION = fun i -> (mcu4 tbl_CLASS_TYPE_DECLARATION "CLASS_TYPE_DECLARATION" (fun i -> (*unique4*) ((((parse_VIRTUAL_FLAG***>parse_CLASS_TYPE_PARAMETERS***>(term_to_parser "?LIDENT?")***>(term_to_parser "?EQUAL?")***>parse_CLASS_SIGNATURE***>parse_POST_ITEM_ATTRIBUTES) >>>> ( fIXME ))) i)) i)

 and parse_CLASS_TYPE_PARAMETERS = fun i -> (mcu4 tbl_CLASS_TYPE_PARAMETERS "CLASS_TYPE_PARAMETERS" (fun i -> (*unique4*) (((((term_to_parser "?eps?")) >>>> ( fIXME ))||||(((term_to_parser "?LBRACKET?")***>parse_TYPE_PARAMETER_LIST***>(term_to_parser "?RBRACKET?")) >>>> ( fIXME ))) i)) i)

 and parse_CLASS_SIGNATURE = fun i -> (mcu4 tbl_CLASS_SIGNATURE "CLASS_SIGNATURE" (fun i -> (*unique4*) (((((term_to_parser "?LBRACKET?")***>parse_CORE_TYPE_COMMA_LIST***>(term_to_parser "?RBRACKET?")***>parse_CLTY_LONGIDENT) >>>> ( fIXME ))||||((parse_CLTY_LONGIDENT) >>>> ( fIXME ))||||(((term_to_parser "?OBJECT?")***>parse_CLASS_SIG_BODY***>(term_to_parser "?END?")) >>>> ( fIXME ))) i)) i)

 and parse_PATTERN_VAR = fun i -> (mcu4 tbl_PATTERN_VAR "PATTERN_VAR" (fun i -> (*unique4*) (((((term_to_parser "?LIDENT?")) >>>> ( fIXME ))||||(((term_to_parser "?UNDERSCORE?")) >>>> ( fIXME ))) i)) i)

 and parse_OPT_DEFAULT = fun i -> (mcu4 tbl_OPT_DEFAULT "OPT_DEFAULT" (fun i -> (*unique4*) (((((term_to_parser "?eps?")) >>>> ( fIXME ))||||(((term_to_parser "?EQUAL?")***>parse_SEQ_EXPR) >>>> ( fIXME ))) i)) i)

 and parse_LABEL_LET_PATTERN = fun i -> (mcu4 tbl_LABEL_LET_PATTERN "LABEL_LET_PATTERN" (fun i -> (*unique4*) ((((parse_LABEL_VAR) >>>> ( fIXME ))||||((parse_LABEL_VAR***>(term_to_parser "?COLON?")***>parse_CORE_TYPE) >>>> ( fIXME ))) i)) i)

 and parse_LABEL_VAR = fun i -> (mcu4 tbl_LABEL_VAR "LABEL_VAR" (fun i -> (*unique4*) (((((term_to_parser "?LIDENT?")) >>>> ( fIXME ))) i)) i)

 and parse_LET_PATTERN = fun i -> (mcu4 tbl_LET_PATTERN "LET_PATTERN" (fun i -> (*unique4*) ((((parse_PATTERN) >>>> ( fIXME ))||||((parse_PATTERN***>(term_to_parser "?COLON?")***>parse_CORE_TYPE) >>>> ( fIXME ))) i)) i)

 and parse_MODULE_BINDING_BODY = fun i -> (mcu4 tbl_MODULE_BINDING_BODY "MODULE_BINDING_BODY" (fun i -> (*unique4*) (((((term_to_parser "?EQUAL?")***>parse_MODULE_EXPR) >>>> ( fIXME ))||||(((term_to_parser "?COLON?")***>parse_MODULE_TYPE***>(term_to_parser "?EQUAL?")***>parse_MODULE_EXPR) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>(term_to_parser "?UIDENT?")***>(term_to_parser "?COLON?")***>parse_MODULE_TYPE***>(term_to_parser "?RPAREN?")***>parse_MODULE_BINDING_BODY) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>(term_to_parser "?RPAREN?")***>parse_MODULE_BINDING_BODY) >>>> ( fIXME ))) i)) i)

 and parse_CLASS_STRUCTURE = fun i -> (mcu4 tbl_CLASS_STRUCTURE "CLASS_STRUCTURE" (fun i -> (*unique4*) ((((parse_CLASS_SELF_PATTERN***>parse_CLASS_FIELDS) >>>> ( fIXME ))) i)) i)

 and parse_MODULE_EXPR = fun i -> (mcu4 tbl_MODULE_EXPR "MODULE_EXPR" (fun i -> (*unique4*) ((((parse_MOD_LONGIDENT) >>>> ( fIXME ))||||(((term_to_parser "?STRUCT?")***>parse_STRUCTURE***>(term_to_parser "?END?")) >>>> ( fIXME ))||||(((term_to_parser "?FUNCTOR?")***>(term_to_parser "?LPAREN?")***>(term_to_parser "?UIDENT?")***>(term_to_parser "?COLON?")***>parse_MODULE_TYPE***>(term_to_parser "?RPAREN?")***>(term_to_parser "?MINUSGREATER?")***>parse_MODULE_EXPR) >>>> ( fIXME ))||||(((term_to_parser "?FUNCTOR?")***>(term_to_parser "?LPAREN?")***>(term_to_parser "?RPAREN?")***>(term_to_parser "?MINUSGREATER?")***>parse_MODULE_EXPR) >>>> ( fIXME ))||||((parse_MODULE_EXPR***>(term_to_parser "?LPAREN?")***>parse_MODULE_EXPR***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||((parse_MODULE_EXPR***>(term_to_parser "?LPAREN?")***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>parse_MODULE_EXPR***>(term_to_parser "?COLON?")***>parse_MODULE_TYPE***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>parse_MODULE_EXPR***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>(term_to_parser "?VAL?")***>parse_EXPR***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>(term_to_parser "?VAL?")***>parse_EXPR***>(term_to_parser "?COLON?")***>parse_PACKAGE_TYPE***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>(term_to_parser "?VAL?")***>parse_EXPR***>(term_to_parser "?COLON?")***>parse_PACKAGE_TYPE***>(term_to_parser "?COLONGREATER?")***>parse_PACKAGE_TYPE***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>(term_to_parser "?VAL?")***>parse_EXPR***>(term_to_parser "?COLONGREATER?")***>parse_PACKAGE_TYPE***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||((parse_MODULE_EXPR***>parse_ATTRIBUTE) >>>> ( fIXME ))||||((parse_EXTENSION) >>>> ( fIXME ))) i)) i)

 and parse_SIMPLE_LABELED_EXPR_LIST = fun i -> (mcu4 tbl_SIMPLE_LABELED_EXPR_LIST "SIMPLE_LABELED_EXPR_LIST" (fun i -> (*unique4*) ((((parse_LABELED_SIMPLE_EXPR) >>>> ( fIXME ))||||((parse_SIMPLE_LABELED_EXPR_LIST***>parse_LABELED_SIMPLE_EXPR) >>>> ( fIXME ))) i)) i)

 and parse_LABELED_SIMPLE_EXPR = fun i -> (mcu4 tbl_LABELED_SIMPLE_EXPR "LABELED_SIMPLE_EXPR" (fun i -> (*unique4*) ((((parse_SIMPLE_EXPR) >>>> ( fIXME ))||||((parse_LABEL_EXPR) >>>> ( fIXME ))) i)) i)

 and parse_LABEL_EXPR = fun i -> (mcu4 tbl_LABEL_EXPR "LABEL_EXPR" (fun i -> (*unique4*) (((((term_to_parser "?LABEL?")***>parse_SIMPLE_EXPR) >>>> ( fIXME ))||||(((term_to_parser "?TILDE?")***>parse_LABEL_IDENT) >>>> ( fIXME ))||||(((term_to_parser "?QUESTION?")***>parse_LABEL_IDENT) >>>> ( fIXME ))||||(((term_to_parser "?OPTLABEL?")***>parse_SIMPLE_EXPR) >>>> ( fIXME ))) i)) i)

 and parse_LABEL_IDENT = fun i -> (mcu4 tbl_LABEL_IDENT "LABEL_IDENT" (fun i -> (*unique4*) (((((term_to_parser "?LIDENT?")) >>>> ( fIXME ))) i)) i)

 and parse_LET_BINDINGS = fun i -> (mcu4 tbl_LET_BINDINGS "LET_BINDINGS" (fun i -> (*unique4*) ((((parse_LET_BINDING) >>>> ( fIXME ))||||((parse_LET_BINDINGS***>(term_to_parser "?AND?")***>parse_LET_BINDING) >>>> ( fIXME ))) i)) i)

 and parse_LET_BINDING = fun i -> (mcu4 tbl_LET_BINDING "LET_BINDING" (fun i -> (*unique4*) ((((parse_LET_BINDING_***>parse_POST_ITEM_ATTRIBUTES) >>>> ( fIXME ))) i)) i)

 and parse_LIDENT_LIST = fun i -> (mcu4 tbl_LIDENT_LIST "LIDENT_LIST" (fun i -> (*unique4*) (((((term_to_parser "?LIDENT?")) >>>> ( fIXME ))||||(((term_to_parser "?LIDENT?")***>parse_LIDENT_LIST) >>>> ( fIXME ))) i)) i)

 and parse_LET_BINDING_ = fun i -> (mcu4 tbl_LET_BINDING_ "LET_BINDING_" (fun i -> (*unique4*) ((((parse_VAL_IDENT***>parse_FUN_BINDING) >>>> ( fIXME ))||||((parse_VAL_IDENT***>(term_to_parser "?COLON?")***>parse_TYPEVAR_LIST***>(term_to_parser "?DOT?")***>parse_CORE_TYPE***>(term_to_parser "?EQUAL?")***>parse_SEQ_EXPR) >>>> ( fIXME ))||||((parse_VAL_IDENT***>(term_to_parser "?COLON?")***>(term_to_parser "?TYPE?")***>parse_LIDENT_LIST***>(term_to_parser "?DOT?")***>parse_CORE_TYPE***>(term_to_parser "?EQUAL?")***>parse_SEQ_EXPR) >>>> ( fIXME ))||||((parse_PATTERN***>(term_to_parser "?EQUAL?")***>parse_SEQ_EXPR) >>>> ( fIXME ))||||((parse_SIMPLE_PATTERN_NOT_IDENT***>(term_to_parser "?COLON?")***>parse_CORE_TYPE***>(term_to_parser "?EQUAL?")***>parse_SEQ_EXPR) >>>> ( fIXME ))) i)) i)

 and parse_STRICT_BINDING = fun i -> (mcu4 tbl_STRICT_BINDING "STRICT_BINDING" (fun i -> (*unique4*) (((((term_to_parser "?EQUAL?")***>parse_SEQ_EXPR) >>>> ( fIXME ))||||((parse_LABELED_SIMPLE_PATTERN***>parse_FUN_BINDING) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>(term_to_parser "?TYPE?")***>(term_to_parser "?LIDENT?")***>(term_to_parser "?RPAREN?")***>parse_FUN_BINDING) >>>> ( fIXME ))) i)) i)

 and parse_FUN_BINDING = fun i -> (mcu4 tbl_FUN_BINDING "FUN_BINDING" (fun i -> (*unique4*) ((((parse_STRICT_BINDING) >>>> ( fIXME ))||||((parse_TYPE_CONSTRAINT***>(term_to_parser "?EQUAL?")***>parse_SEQ_EXPR) >>>> ( fIXME ))) i)) i)

 and parse_MATCH_CASES = fun i -> (mcu4 tbl_MATCH_CASES "MATCH_CASES" (fun i -> (*unique4*) ((((parse_MATCH_CASE) >>>> ( fIXME ))||||((parse_MATCH_CASES***>(term_to_parser "?BAR?")***>parse_MATCH_CASE) >>>> ( fIXME ))) i)) i)

 and parse_MATCH_CASE = fun i -> (mcu4 tbl_MATCH_CASE "MATCH_CASE" (fun i -> (*unique4*) ((((parse_PATTERN***>(term_to_parser "?MINUSGREATER?")***>parse_SEQ_EXPR) >>>> ( fIXME ))||||((parse_PATTERN***>(term_to_parser "?WHEN?")***>parse_SEQ_EXPR***>(term_to_parser "?MINUSGREATER?")***>parse_SEQ_EXPR) >>>> ( fIXME ))) i)) i)

 and parse_LABELED_SIMPLE_PATTERN = fun i -> (mcu4 tbl_LABELED_SIMPLE_PATTERN "LABELED_SIMPLE_PATTERN" (fun i -> (*unique4*) (((((term_to_parser "?QUESTION?")***>(term_to_parser "?LPAREN?")***>parse_LABEL_LET_PATTERN***>parse_OPT_DEFAULT***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||(((term_to_parser "?QUESTION?")***>parse_LABEL_VAR) >>>> ( fIXME ))||||(((term_to_parser "?OPTLABEL?")***>(term_to_parser "?LPAREN?")***>parse_LET_PATTERN***>parse_OPT_DEFAULT***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||(((term_to_parser "?OPTLABEL?")***>parse_PATTERN_VAR) >>>> ( fIXME ))||||(((term_to_parser "?TILDE?")***>(term_to_parser "?LPAREN?")***>parse_LABEL_LET_PATTERN***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||(((term_to_parser "?TILDE?")***>parse_LABEL_VAR) >>>> ( fIXME ))||||(((term_to_parser "?LABEL?")***>parse_SIMPLE_PATTERN) >>>> ( fIXME ))||||((parse_SIMPLE_PATTERN) >>>> ( fIXME ))) i)) i)

 and parse_FUN_DEF = fun i -> (mcu4 tbl_FUN_DEF "FUN_DEF" (fun i -> (*unique4*) (((((term_to_parser "?MINUSGREATER?")***>parse_SEQ_EXPR) >>>> ( fIXME ))||||((parse_LABELED_SIMPLE_PATTERN***>parse_FUN_DEF) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>(term_to_parser "?TYPE?")***>(term_to_parser "?LIDENT?")***>(term_to_parser "?RPAREN?")***>parse_FUN_DEF) >>>> ( fIXME ))) i)) i)

 and parse_EXPR_COMMA_LIST = fun i -> (mcu4 tbl_EXPR_COMMA_LIST "EXPR_COMMA_LIST" (fun i -> (*unique4*) ((((parse_EXPR_COMMA_LIST***>(term_to_parser "?COMMA?")***>parse_EXPR) >>>> ( fIXME ))||||((parse_EXPR***>(term_to_parser "?COMMA?")***>parse_EXPR) >>>> ( fIXME ))) i)) i)

 and parse_SIMPLE_EXPR = fun i -> (mcu4 tbl_SIMPLE_EXPR "SIMPLE_EXPR" (fun i -> (*unique4*) ((((parse_VAL_LONGIDENT) >>>> ( fIXME ))||||((parse_CONSTANT) >>>> ( fIXME ))||||((parse_CONSTR_LONGIDENT) >>>> ( fIXME ))||||((parse_NAME_TAG) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>parse_SEQ_EXPR***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||(((term_to_parser "?BEGIN?")***>parse_EXT_ATTRIBUTES***>parse_SEQ_EXPR***>(term_to_parser "?END?")) >>>> ( fIXME ))||||(((term_to_parser "?BEGIN?")***>parse_EXT_ATTRIBUTES***>(term_to_parser "?END?")) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>parse_SEQ_EXPR***>parse_TYPE_CONSTRAINT***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||((parse_SIMPLE_EXPR***>(term_to_parser "?DOT?")***>parse_LABEL_LONGIDENT) >>>> ( fIXME ))||||((parse_MOD_LONGIDENT***>(term_to_parser "?DOT?")***>(term_to_parser "?LPAREN?")***>parse_SEQ_EXPR***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||((parse_SIMPLE_EXPR***>(term_to_parser "?DOT?")***>(term_to_parser "?LPAREN?")***>parse_SEQ_EXPR***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||((parse_SIMPLE_EXPR***>(term_to_parser "?DOT?")***>(term_to_parser "?LBRACKET?")***>parse_SEQ_EXPR***>(term_to_parser "?RBRACKET?")) >>>> ( fIXME ))||||((parse_SIMPLE_EXPR***>(term_to_parser "?DOT?")***>(term_to_parser "?LBRACE?")***>parse_EXPR***>(term_to_parser "?RBRACE?")) >>>> ( fIXME ))||||(((term_to_parser "?LBRACE?")***>parse_RECORD_EXPR***>(term_to_parser "?RBRACE?")) >>>> ( fIXME ))||||(((term_to_parser "?LBRACKETBAR?")***>parse_EXPR_SEMI_LIST***>parse_OPT_SEMI***>(term_to_parser "?BARRBRACKET?")) >>>> ( fIXME ))||||(((term_to_parser "?LBRACKETBAR?")***>(term_to_parser "?BARRBRACKET?")) >>>> ( fIXME ))||||(((term_to_parser "?LBRACKET?")***>parse_EXPR_SEMI_LIST***>parse_OPT_SEMI***>(term_to_parser "?RBRACKET?")) >>>> ( fIXME ))||||(((term_to_parser "?PREFIXOP?")***>parse_SIMPLE_EXPR) >>>> ( fIXME ))||||(((term_to_parser "?BANG?")***>parse_SIMPLE_EXPR) >>>> ( fIXME ))||||(((term_to_parser "?NEW?")***>parse_EXT_ATTRIBUTES***>parse_CLASS_LONGIDENT) >>>> ( fIXME ))||||(((term_to_parser "?LBRACELESS?")***>parse_FIELD_EXPR_LIST***>parse_OPT_SEMI***>(term_to_parser "?GREATERRBRACE?")) >>>> ( fIXME ))||||(((term_to_parser "?LBRACELESS?")***>(term_to_parser "?GREATERRBRACE?")) >>>> ( fIXME ))||||((parse_SIMPLE_EXPR***>(term_to_parser "?SHARP?")***>parse_LABEL) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>(term_to_parser "?MODULE?")***>parse_MODULE_EXPR***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>(term_to_parser "?MODULE?")***>parse_MODULE_EXPR***>(term_to_parser "?COLON?")***>parse_PACKAGE_TYPE***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||((parse_EXTENSION) >>>> ( fIXME ))) i)) i)

 and parse_RECORD_EXPR = fun i -> (mcu4 tbl_RECORD_EXPR "RECORD_EXPR" (fun i -> (*unique4*) ((((parse_SIMPLE_EXPR***>(term_to_parser "?WITH?")***>parse_LBL_EXPR_LIST) >>>> ( fIXME ))||||((parse_LBL_EXPR_LIST) >>>> ( fIXME ))) i)) i)

 and parse_LBL_EXPR_LIST = fun i -> (mcu4 tbl_LBL_EXPR_LIST "LBL_EXPR_LIST" (fun i -> (*unique4*) ((((parse_LBL_EXPR) >>>> ( fIXME ))||||((parse_LBL_EXPR***>(term_to_parser "?SEMI?")***>parse_LBL_EXPR_LIST) >>>> ( fIXME ))||||((parse_LBL_EXPR***>(term_to_parser "?SEMI?")) >>>> ( fIXME ))) i)) i)

 and parse_LBL_EXPR = fun i -> (mcu4 tbl_LBL_EXPR "LBL_EXPR" (fun i -> (*unique4*) ((((parse_LABEL_LONGIDENT***>(term_to_parser "?EQUAL?")***>parse_EXPR) >>>> ( fIXME ))||||((parse_LABEL_LONGIDENT) >>>> ( fIXME ))) i)) i)

 and parse_FIELD_EXPR_LIST = fun i -> (mcu4 tbl_FIELD_EXPR_LIST "FIELD_EXPR_LIST" (fun i -> (*unique4*) ((((parse_LABEL***>(term_to_parser "?EQUAL?")***>parse_EXPR) >>>> ( fIXME ))||||((parse_FIELD_EXPR_LIST***>(term_to_parser "?SEMI?")***>parse_LABEL***>(term_to_parser "?EQUAL?")***>parse_EXPR) >>>> ( fIXME ))) i)) i)

 and parse_EXPR_SEMI_LIST = fun i -> (mcu4 tbl_EXPR_SEMI_LIST "EXPR_SEMI_LIST" (fun i -> (*unique4*) ((((parse_EXPR) >>>> ( fIXME ))||||((parse_EXPR_SEMI_LIST***>(term_to_parser "?SEMI?")***>parse_EXPR) >>>> ( fIXME ))) i)) i)

 and parse_EXPR = fun i -> (mcu4 tbl_EXPR "EXPR" (fun i -> (*unique4*) ((((parse_SIMPLE_EXPR) >>>> ( fIXME ))||||((parse_SIMPLE_EXPR***>parse_SIMPLE_LABELED_EXPR_LIST) >>>> ( fIXME ))||||(((term_to_parser "?LET?")***>parse_EXT_ATTRIBUTES***>parse_REC_FLAG***>parse_LET_BINDINGS***>(term_to_parser "?IN?")***>parse_SEQ_EXPR) >>>> ( fIXME ))||||(((term_to_parser "?LET?")***>(term_to_parser "?MODULE?")***>parse_EXT_ATTRIBUTES***>(term_to_parser "?UIDENT?")***>parse_MODULE_BINDING_BODY***>(term_to_parser "?IN?")***>parse_SEQ_EXPR) >>>> ( fIXME ))||||(((term_to_parser "?LET?")***>(term_to_parser "?OPEN?")***>parse_OVERRIDE_FLAG***>parse_EXT_ATTRIBUTES***>parse_MOD_LONGIDENT***>(term_to_parser "?IN?")***>parse_SEQ_EXPR) >>>> ( fIXME ))||||(((term_to_parser "?FUNCTION?")***>parse_EXT_ATTRIBUTES***>parse_OPT_BAR***>parse_MATCH_CASES) >>>> ( fIXME ))||||(((term_to_parser "?FUN?")***>parse_EXT_ATTRIBUTES***>parse_LABELED_SIMPLE_PATTERN***>parse_FUN_DEF) >>>> ( fIXME ))||||(((term_to_parser "?FUN?")***>parse_EXT_ATTRIBUTES***>(term_to_parser "?LPAREN?")***>(term_to_parser "?TYPE?")***>(term_to_parser "?LIDENT?")***>(term_to_parser "?RPAREN?")***>parse_FUN_DEF) >>>> ( fIXME ))||||(((term_to_parser "?MATCH?")***>parse_EXT_ATTRIBUTES***>parse_SEQ_EXPR***>(term_to_parser "?WITH?")***>parse_OPT_BAR***>parse_MATCH_CASES) >>>> ( fIXME ))||||(((term_to_parser "?TRY?")***>parse_EXT_ATTRIBUTES***>parse_SEQ_EXPR***>(term_to_parser "?WITH?")***>parse_OPT_BAR***>parse_MATCH_CASES) >>>> ( fIXME ))||||((parse_EXPR_COMMA_LIST) >>>> ( fIXME ))||||((parse_CONSTR_LONGIDENT***>parse_SIMPLE_EXPR) >>>> ( fIXME ))||||((parse_NAME_TAG***>parse_SIMPLE_EXPR) >>>> ( fIXME ))||||(((term_to_parser "?IF?")***>parse_EXT_ATTRIBUTES***>parse_SEQ_EXPR***>(term_to_parser "?THEN?")***>parse_EXPR***>(term_to_parser "?ELSE?")***>parse_EXPR) >>>> ( fIXME ))||||(((term_to_parser "?IF?")***>parse_EXT_ATTRIBUTES***>parse_SEQ_EXPR***>(term_to_parser "?THEN?")***>parse_EXPR) >>>> ( fIXME ))||||(((term_to_parser "?WHILE?")***>parse_EXT_ATTRIBUTES***>parse_SEQ_EXPR***>(term_to_parser "?DO?")***>parse_SEQ_EXPR***>(term_to_parser "?DONE?")) >>>> ( fIXME ))||||(((term_to_parser "?FOR?")***>parse_EXT_ATTRIBUTES***>parse_PATTERN***>(term_to_parser "?EQUAL?")***>parse_SEQ_EXPR***>parse_DIRECTION_FLAG***>parse_SEQ_EXPR***>(term_to_parser "?DO?")***>parse_SEQ_EXPR***>(term_to_parser "?DONE?")) >>>> ( fIXME ))||||((parse_EXPR***>(term_to_parser "?COLONCOLON?")***>parse_EXPR) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>(term_to_parser "?COLONCOLON?")***>(term_to_parser "?RPAREN?")***>(term_to_parser "?LPAREN?")***>parse_EXPR***>(term_to_parser "?COMMA?")***>parse_EXPR***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||((parse_EXPR***>(term_to_parser "?INFIXOP0?")***>parse_EXPR) >>>> ( fIXME ))||||((parse_EXPR***>(term_to_parser "?INFIXOP1?")***>parse_EXPR) >>>> ( fIXME ))||||((parse_EXPR***>(term_to_parser "?INFIXOP2?")***>parse_EXPR) >>>> ( fIXME ))||||((parse_EXPR***>(term_to_parser "?INFIXOP3?")***>parse_EXPR) >>>> ( fIXME ))||||((parse_EXPR***>(term_to_parser "?INFIXOP4?")***>parse_EXPR) >>>> ( fIXME ))||||((parse_EXPR***>(term_to_parser "?PLUS?")***>parse_EXPR) >>>> ( fIXME ))||||((parse_EXPR***>(term_to_parser "?PLUSDOT?")***>parse_EXPR) >>>> ( fIXME ))||||((parse_EXPR***>(term_to_parser "?MINUS?")***>parse_EXPR) >>>> ( fIXME ))||||((parse_EXPR***>(term_to_parser "?MINUSDOT?")***>parse_EXPR) >>>> ( fIXME ))||||((parse_EXPR***>(term_to_parser "?STAR?")***>parse_EXPR) >>>> ( fIXME ))||||((parse_EXPR***>(term_to_parser "?PERCENT?")***>parse_EXPR) >>>> ( fIXME ))||||((parse_EXPR***>(term_to_parser "?EQUAL?")***>parse_EXPR) >>>> ( fIXME ))||||((parse_EXPR***>(term_to_parser "?LESS?")***>parse_EXPR) >>>> ( fIXME ))||||((parse_EXPR***>(term_to_parser "?GREATER?")***>parse_EXPR) >>>> ( fIXME ))||||((parse_EXPR***>(term_to_parser "?OR?")***>parse_EXPR) >>>> ( fIXME ))||||((parse_EXPR***>(term_to_parser "?BARBAR?")***>parse_EXPR) >>>> ( fIXME ))||||((parse_EXPR***>(term_to_parser "?AMPERSAND?")***>parse_EXPR) >>>> ( fIXME ))||||((parse_EXPR***>(term_to_parser "?AMPERAMPER?")***>parse_EXPR) >>>> ( fIXME ))||||((parse_EXPR***>(term_to_parser "?COLONEQUAL?")***>parse_EXPR) >>>> ( fIXME ))||||((parse_SUBTRACTIVE***>parse_EXPR) >>>> ( fIXME ))||||((parse_ADDITIVE***>parse_EXPR) >>>> ( fIXME ))||||((parse_SIMPLE_EXPR***>(term_to_parser "?DOT?")***>parse_LABEL_LONGIDENT***>(term_to_parser "?LESSMINUS?")***>parse_EXPR) >>>> ( fIXME ))||||((parse_SIMPLE_EXPR***>(term_to_parser "?DOT?")***>(term_to_parser "?LPAREN?")***>parse_SEQ_EXPR***>(term_to_parser "?RPAREN?")***>(term_to_parser "?LESSMINUS?")***>parse_EXPR) >>>> ( fIXME ))||||((parse_SIMPLE_EXPR***>(term_to_parser "?DOT?")***>(term_to_parser "?LBRACKET?")***>parse_SEQ_EXPR***>(term_to_parser "?RBRACKET?")***>(term_to_parser "?LESSMINUS?")***>parse_EXPR) >>>> ( fIXME ))||||((parse_SIMPLE_EXPR***>(term_to_parser "?DOT?")***>(term_to_parser "?LBRACE?")***>parse_EXPR***>(term_to_parser "?RBRACE?")***>(term_to_parser "?LESSMINUS?")***>parse_EXPR) >>>> ( fIXME ))||||((parse_LABEL***>(term_to_parser "?LESSMINUS?")***>parse_EXPR) >>>> ( fIXME ))||||(((term_to_parser "?ASSERT?")***>parse_EXT_ATTRIBUTES***>parse_SIMPLE_EXPR) >>>> ( fIXME ))||||(((term_to_parser "?LAZY?")***>parse_EXT_ATTRIBUTES***>parse_SIMPLE_EXPR) >>>> ( fIXME ))||||(((term_to_parser "?OBJECT?")***>parse_EXT_ATTRIBUTES***>parse_CLASS_STRUCTURE***>(term_to_parser "?END?")) >>>> ( fIXME ))||||((parse_EXPR***>parse_ATTRIBUTE) >>>> ( fIXME ))) i)) i)

 and parse_TYPE_CONSTRAINT = fun i -> (mcu4 tbl_TYPE_CONSTRAINT "TYPE_CONSTRAINT" (fun i -> (*unique4*) (((((term_to_parser "?COLON?")***>parse_CORE_TYPE) >>>> ( fIXME ))||||(((term_to_parser "?COLON?")***>parse_CORE_TYPE***>(term_to_parser "?COLONGREATER?")***>parse_CORE_TYPE) >>>> ( fIXME ))||||(((term_to_parser "?COLONGREATER?")***>parse_CORE_TYPE) >>>> ( fIXME ))) i)) i)

 and parse_SIMPLE_PATTERN = fun i -> (mcu4 tbl_SIMPLE_PATTERN "SIMPLE_PATTERN" (fun i -> (*unique4*) ((((parse_VAL_IDENT) >>>> ( fIXME ))||||((parse_SIMPLE_PATTERN_NOT_IDENT) >>>> ( fIXME ))) i)) i)

 and parse_SIMPLE_PATTERN_NOT_IDENT = fun i -> (mcu4 tbl_SIMPLE_PATTERN_NOT_IDENT "SIMPLE_PATTERN_NOT_IDENT" (fun i -> (*unique4*) (((((term_to_parser "?UNDERSCORE?")) >>>> ( fIXME ))||||((parse_SIGNED_CONSTANT) >>>> ( fIXME ))||||((parse_SIGNED_CONSTANT***>(term_to_parser "?DOTDOT?")***>parse_SIGNED_CONSTANT) >>>> ( fIXME ))||||((parse_CONSTR_LONGIDENT) >>>> ( fIXME ))||||((parse_NAME_TAG) >>>> ( fIXME ))||||(((term_to_parser "?SHARP?")***>parse_TYPE_LONGIDENT) >>>> ( fIXME ))||||(((term_to_parser "?LBRACE?")***>parse_LBL_PATTERN_LIST***>(term_to_parser "?RBRACE?")) >>>> ( fIXME ))||||(((term_to_parser "?LBRACKET?")***>parse_PATTERN_SEMI_LIST***>parse_OPT_SEMI***>(term_to_parser "?RBRACKET?")) >>>> ( fIXME ))||||(((term_to_parser "?LBRACKETBAR?")***>parse_PATTERN_SEMI_LIST***>parse_OPT_SEMI***>(term_to_parser "?BARRBRACKET?")) >>>> ( fIXME ))||||(((term_to_parser "?LBRACKETBAR?")***>(term_to_parser "?BARRBRACKET?")) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>parse_PATTERN***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>parse_PATTERN***>(term_to_parser "?COLON?")***>parse_CORE_TYPE***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>(term_to_parser "?MODULE?")***>(term_to_parser "?UIDENT?")***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>(term_to_parser "?MODULE?")***>(term_to_parser "?UIDENT?")***>(term_to_parser "?COLON?")***>parse_PACKAGE_TYPE***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||((parse_EXTENSION) >>>> ( fIXME ))) i)) i)

 and parse_PATTERN_COMMA_LIST = fun i -> (mcu4 tbl_PATTERN_COMMA_LIST "PATTERN_COMMA_LIST" (fun i -> (*unique4*) ((((parse_PATTERN_COMMA_LIST***>(term_to_parser "?COMMA?")***>parse_PATTERN) >>>> ( fIXME ))||||((parse_PATTERN***>(term_to_parser "?COMMA?")***>parse_PATTERN) >>>> ( fIXME ))) i)) i)

 and parse_PATTERN_SEMI_LIST = fun i -> (mcu4 tbl_PATTERN_SEMI_LIST "PATTERN_SEMI_LIST" (fun i -> (*unique4*) ((((parse_PATTERN) >>>> ( fIXME ))||||((parse_PATTERN_SEMI_LIST***>(term_to_parser "?SEMI?")***>parse_PATTERN) >>>> ( fIXME ))) i)) i)

 and parse_LBL_PATTERN_LIST = fun i -> (mcu4 tbl_LBL_PATTERN_LIST "LBL_PATTERN_LIST" (fun i -> (*unique4*) ((((parse_LBL_PATTERN) >>>> ( fIXME ))||||((parse_LBL_PATTERN***>(term_to_parser "?SEMI?")) >>>> ( fIXME ))||||((parse_LBL_PATTERN***>(term_to_parser "?SEMI?")***>(term_to_parser "?UNDERSCORE?")***>parse_OPT_SEMI) >>>> ( fIXME ))||||((parse_LBL_PATTERN***>(term_to_parser "?SEMI?")***>parse_LBL_PATTERN_LIST) >>>> ( fIXME ))) i)) i)

 and parse_LBL_PATTERN = fun i -> (mcu4 tbl_LBL_PATTERN "LBL_PATTERN" (fun i -> (*unique4*) ((((parse_LABEL_LONGIDENT***>(term_to_parser "?EQUAL?")***>parse_PATTERN) >>>> ( fIXME ))||||((parse_LABEL_LONGIDENT) >>>> ( fIXME ))) i)) i)

 and parse_PRIMITIVE_DECLARATION = fun i -> (mcu4 tbl_PRIMITIVE_DECLARATION "PRIMITIVE_DECLARATION" (fun i -> (*unique4*) (((((term_to_parser "?STRING?")) >>>> ( fIXME ))||||(((term_to_parser "?STRING?")***>parse_PRIMITIVE_DECLARATION) >>>> ( fIXME ))) i)) i)

 and parse_TYPE_DECLARATIONS = fun i -> (mcu4 tbl_TYPE_DECLARATIONS "TYPE_DECLARATIONS" (fun i -> (*unique4*) ((((parse_TYPE_DECLARATION) >>>> ( fIXME ))||||((parse_TYPE_DECLARATIONS***>(term_to_parser "?AND?")***>parse_TYPE_DECLARATION) >>>> ( fIXME ))) i)) i)

 and parse_TYPE_DECLARATION = fun i -> (mcu4 tbl_TYPE_DECLARATION "TYPE_DECLARATION" (fun i -> (*unique4*) ((((parse_OPTIONAL_TYPE_PARAMETERS***>(term_to_parser "?LIDENT?")***>parse_TYPE_KIND***>parse_CONSTRAINTS***>parse_POST_ITEM_ATTRIBUTES) >>>> ( fIXME ))) i)) i)

 and parse_CONSTRAIN = fun i -> (mcu4 tbl_CONSTRAIN "CONSTRAIN" (fun i -> (*unique4*) ((((parse_CORE_TYPE***>(term_to_parser "?EQUAL?")***>parse_CORE_TYPE) >>>> ( fIXME ))) i)) i)

 and parse_TYPE_KIND = fun i -> (mcu4 tbl_TYPE_KIND "TYPE_KIND" (fun i -> (*unique4*) (((((term_to_parser "?eps?")) >>>> ( fIXME ))||||(((term_to_parser "?EQUAL?")***>parse_CORE_TYPE) >>>> ( fIXME ))||||(((term_to_parser "?EQUAL?")***>(term_to_parser "?PRIVATE?")***>parse_CORE_TYPE) >>>> ( fIXME ))||||(((term_to_parser "?EQUAL?")***>parse_CONSTRUCTOR_DECLARATIONS) >>>> ( fIXME ))||||(((term_to_parser "?EQUAL?")***>(term_to_parser "?PRIVATE?")***>parse_CONSTRUCTOR_DECLARATIONS) >>>> ( fIXME ))||||(((term_to_parser "?EQUAL?")***>parse_PRIVATE_FLAG***>(term_to_parser "?BAR?")***>parse_CONSTRUCTOR_DECLARATIONS) >>>> ( fIXME ))||||(((term_to_parser "?EQUAL?")***>parse_PRIVATE_FLAG***>(term_to_parser "?LBRACE?")***>parse_LABEL_DECLARATIONS***>parse_OPT_SEMI***>(term_to_parser "?RBRACE?")) >>>> ( fIXME ))||||(((term_to_parser "?EQUAL?")***>parse_CORE_TYPE***>(term_to_parser "?EQUAL?")***>parse_PRIVATE_FLAG***>parse_OPT_BAR***>parse_CONSTRUCTOR_DECLARATIONS) >>>> ( fIXME ))||||(((term_to_parser "?EQUAL?")***>parse_CORE_TYPE***>(term_to_parser "?EQUAL?")***>parse_PRIVATE_FLAG***>(term_to_parser "?LBRACE?")***>parse_LABEL_DECLARATIONS***>parse_OPT_SEMI***>(term_to_parser "?RBRACE?")) >>>> ( fIXME ))) i)) i)

 and parse_OPTIONAL_TYPE_PARAMETERS = fun i -> (mcu4 tbl_OPTIONAL_TYPE_PARAMETERS "OPTIONAL_TYPE_PARAMETERS" (fun i -> (*unique4*) (((((term_to_parser "?eps?")) >>>> ( fIXME ))||||((parse_OPTIONAL_TYPE_PARAMETER) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>parse_OPTIONAL_TYPE_PARAMETER_LIST***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))) i)) i)

 and parse_OPTIONAL_TYPE_PARAMETER_LIST = fun i -> (mcu4 tbl_OPTIONAL_TYPE_PARAMETER_LIST "OPTIONAL_TYPE_PARAMETER_LIST" (fun i -> (*unique4*) ((((parse_OPTIONAL_TYPE_PARAMETER) >>>> ( fIXME ))||||((parse_OPTIONAL_TYPE_PARAMETER_LIST***>(term_to_parser "?COMMA?")***>parse_OPTIONAL_TYPE_PARAMETER) >>>> ( fIXME ))) i)) i)

 and parse_OPTIONAL_TYPE_PARAMETER = fun i -> (mcu4 tbl_OPTIONAL_TYPE_PARAMETER "OPTIONAL_TYPE_PARAMETER" (fun i -> (*unique4*) ((((parse_TYPE_VARIANCE***>(term_to_parser "?QUOTE?")***>parse_IDENT) >>>> ( fIXME ))||||((parse_TYPE_VARIANCE***>(term_to_parser "?UNDERSCORE?")) >>>> ( fIXME ))) i)) i)

 and parse_TYPE_VARIANCE = fun i -> (mcu4 tbl_TYPE_VARIANCE "TYPE_VARIANCE" (fun i -> (*unique4*) (((((term_to_parser "?eps?")) >>>> ( fIXME ))||||(((term_to_parser "?PLUS?")) >>>> ( fIXME ))||||(((term_to_parser "?MINUS?")) >>>> ( fIXME ))) i)) i)

 and parse_TYPE_PARAMETER_LIST = fun i -> (mcu4 tbl_TYPE_PARAMETER_LIST "TYPE_PARAMETER_LIST" (fun i -> (*unique4*) ((((parse_TYPE_PARAMETER) >>>> ( fIXME ))||||((parse_TYPE_PARAMETER_LIST***>(term_to_parser "?COMMA?")***>parse_TYPE_PARAMETER) >>>> ( fIXME ))) i)) i)

 and parse_TYPE_PARAMETER = fun i -> (mcu4 tbl_TYPE_PARAMETER "TYPE_PARAMETER" (fun i -> (*unique4*) ((((parse_TYPE_VARIANCE***>(term_to_parser "?QUOTE?")***>parse_IDENT) >>>> ( fIXME ))) i)) i)

 and parse_CONSTRUCTOR_DECLARATIONS = fun i -> (mcu4 tbl_CONSTRUCTOR_DECLARATIONS "CONSTRUCTOR_DECLARATIONS" (fun i -> (*unique4*) ((((parse_CONSTRUCTOR_DECLARATION) >>>> ( fIXME ))||||((parse_CONSTRUCTOR_DECLARATIONS***>(term_to_parser "?BAR?")***>parse_CONSTRUCTOR_DECLARATION) >>>> ( fIXME ))) i)) i)

 and parse_EXCEPTION_DECLARATION = fun i -> (mcu4 tbl_EXCEPTION_DECLARATION "EXCEPTION_DECLARATION" (fun i -> (*unique4*) ((((parse_CONSTRUCTOR_DECLARATION***>parse_POST_ITEM_ATTRIBUTES) >>>> ( fIXME ))) i)) i)

 and parse_CONSTRUCTOR_DECLARATION = fun i -> (mcu4 tbl_CONSTRUCTOR_DECLARATION "CONSTRUCTOR_DECLARATION" (fun i -> (*unique4*) ((((parse_CONSTR_IDENT***>parse_ATTRIBUTES***>parse_GENERALIZED_CONSTRUCTOR_ARGUMENTS) >>>> ( fIXME ))) i)) i)

 and parse_GENERALIZED_CONSTRUCTOR_ARGUMENTS = fun i -> (mcu4 tbl_GENERALIZED_CONSTRUCTOR_ARGUMENTS "GENERALIZED_CONSTRUCTOR_ARGUMENTS" (fun i -> (*unique4*) (((((term_to_parser "?eps?")) >>>> ( fIXME ))||||(((term_to_parser "?OF?")***>parse_CORE_TYPE_LIST) >>>> ( fIXME ))||||(((term_to_parser "?COLON?")***>parse_CORE_TYPE_LIST***>(term_to_parser "?MINUSGREATER?")***>parse_SIMPLE_CORE_TYPE) >>>> ( fIXME ))||||(((term_to_parser "?COLON?")***>parse_SIMPLE_CORE_TYPE) >>>> ( fIXME ))) i)) i)

 and parse_LABEL_DECLARATIONS = fun i -> (mcu4 tbl_LABEL_DECLARATIONS "LABEL_DECLARATIONS" (fun i -> (*unique4*) ((((parse_LABEL_DECLARATION) >>>> ( fIXME ))||||((parse_LABEL_DECLARATIONS***>(term_to_parser "?SEMI?")***>parse_LABEL_DECLARATION) >>>> ( fIXME ))) i)) i)

 and parse_LABEL_DECLARATION = fun i -> (mcu4 tbl_LABEL_DECLARATION "LABEL_DECLARATION" (fun i -> (*unique4*) ((((parse_MUTABLE_FLAG***>parse_LABEL***>parse_ATTRIBUTES***>(term_to_parser "?COLON?")***>parse_POLY_TYPE) >>>> ( fIXME ))) i)) i)

 and parse_WITH_CONSTRAINTS = fun i -> (mcu4 tbl_WITH_CONSTRAINTS "WITH_CONSTRAINTS" (fun i -> (*unique4*) ((((parse_WITH_CONSTRAINT) >>>> ( fIXME ))||||((parse_WITH_CONSTRAINTS***>(term_to_parser "?AND?")***>parse_WITH_CONSTRAINT) >>>> ( fIXME ))) i)) i)

 and parse_CONSTRAINTS = fun i -> (mcu4 tbl_CONSTRAINTS "CONSTRAINTS" (fun i -> (*unique4*) ((((parse_CONSTRAINTS***>(term_to_parser "?CONSTRAINT?")***>parse_CONSTRAIN) >>>> ( fIXME ))||||(((term_to_parser "?eps?")) >>>> ( fIXME ))) i)) i)

 and parse_TYPE_PARAMETERS = fun i -> (mcu4 tbl_TYPE_PARAMETERS "TYPE_PARAMETERS" (fun i -> (*unique4*) (((((term_to_parser "?eps?")) >>>> ( fIXME ))||||((parse_TYPE_PARAMETER) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>parse_TYPE_PARAMETER_LIST***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))) i)) i)

 and parse_WITH_CONSTRAINT = fun i -> (mcu4 tbl_WITH_CONSTRAINT "WITH_CONSTRAINT" (fun i -> (*unique4*) (((((term_to_parser "?TYPE?")***>parse_TYPE_PARAMETERS***>parse_LABEL_LONGIDENT***>parse_WITH_TYPE_BINDER***>parse_CORE_TYPE***>parse_CONSTRAINTS) >>>> ( fIXME ))||||(((term_to_parser "?TYPE?")***>parse_TYPE_PARAMETERS***>parse_LABEL***>(term_to_parser "?COLONEQUAL?")***>parse_CORE_TYPE) >>>> ( fIXME ))||||(((term_to_parser "?MODULE?")***>parse_MOD_LONGIDENT***>(term_to_parser "?EQUAL?")***>parse_MOD_EXT_LONGIDENT) >>>> ( fIXME ))||||(((term_to_parser "?MODULE?")***>(term_to_parser "?UIDENT?")***>(term_to_parser "?COLONEQUAL?")***>parse_MOD_EXT_LONGIDENT) >>>> ( fIXME ))) i)) i)

 and parse_WITH_TYPE_BINDER = fun i -> (mcu4 tbl_WITH_TYPE_BINDER "WITH_TYPE_BINDER" (fun i -> (*unique4*) (((((term_to_parser "?EQUAL?")) >>>> ( fIXME ))||||(((term_to_parser "?EQUAL?")***>(term_to_parser "?PRIVATE?")) >>>> ( fIXME ))) i)) i)

 and parse_TYPEVAR_LIST = fun i -> (mcu4 tbl_TYPEVAR_LIST "TYPEVAR_LIST" (fun i -> (*unique4*) (((((term_to_parser "?QUOTE?")***>parse_IDENT) >>>> ( fIXME ))||||((parse_TYPEVAR_LIST***>(term_to_parser "?QUOTE?")***>parse_IDENT) >>>> ( fIXME ))) i)) i)

 and parse_CORE_TYPE2 = fun i -> (mcu4 tbl_CORE_TYPE2 "CORE_TYPE2" (fun i -> (*unique4*) ((((parse_SIMPLE_CORE_TYPE_OR_TUPLE) >>>> ( fIXME ))||||(((term_to_parser "?QUESTION?")***>(term_to_parser "?LIDENT?")***>(term_to_parser "?COLON?")***>parse_CORE_TYPE2***>(term_to_parser "?MINUSGREATER?")***>parse_CORE_TYPE2) >>>> ( fIXME ))||||(((term_to_parser "?OPTLABEL?")***>parse_CORE_TYPE2***>(term_to_parser "?MINUSGREATER?")***>parse_CORE_TYPE2) >>>> ( fIXME ))||||(((term_to_parser "?LIDENT?")***>(term_to_parser "?COLON?")***>parse_CORE_TYPE2***>(term_to_parser "?MINUSGREATER?")***>parse_CORE_TYPE2) >>>> ( fIXME ))||||((parse_CORE_TYPE2***>(term_to_parser "?MINUSGREATER?")***>parse_CORE_TYPE2) >>>> ( fIXME ))) i)) i)

 and parse_SIMPLE_CORE_TYPE2 = fun i -> (mcu4 tbl_SIMPLE_CORE_TYPE2 "SIMPLE_CORE_TYPE2" (fun i -> (*unique4*) (((((term_to_parser "?QUOTE?")***>parse_IDENT) >>>> ( fIXME ))||||(((term_to_parser "?UNDERSCORE?")) >>>> ( fIXME ))||||((parse_TYPE_LONGIDENT) >>>> ( fIXME ))||||((parse_SIMPLE_CORE_TYPE2***>parse_TYPE_LONGIDENT) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>parse_CORE_TYPE_COMMA_LIST***>(term_to_parser "?RPAREN?")***>parse_TYPE_LONGIDENT) >>>> ( fIXME ))||||(((term_to_parser "?LESS?")***>parse_METH_LIST***>(term_to_parser "?GREATER?")) >>>> ( fIXME ))||||(((term_to_parser "?LESS?")***>(term_to_parser "?GREATER?")) >>>> ( fIXME ))||||(((term_to_parser "?SHARP?")***>parse_CLASS_LONGIDENT) >>>> ( fIXME ))||||((parse_SIMPLE_CORE_TYPE2***>(term_to_parser "?SHARP?")***>parse_CLASS_LONGIDENT) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>parse_CORE_TYPE_COMMA_LIST***>(term_to_parser "?RPAREN?")***>(term_to_parser "?SHARP?")***>parse_CLASS_LONGIDENT) >>>> ( fIXME ))||||(((term_to_parser "?LBRACKET?")***>parse_TAG_FIELD***>(term_to_parser "?RBRACKET?")) >>>> ( fIXME ))||||(((term_to_parser "?LBRACKET?")***>(term_to_parser "?BAR?")***>parse_ROW_FIELD_LIST***>(term_to_parser "?RBRACKET?")) >>>> ( fIXME ))||||(((term_to_parser "?LBRACKET?")***>parse_ROW_FIELD***>(term_to_parser "?BAR?")***>parse_ROW_FIELD_LIST***>(term_to_parser "?RBRACKET?")) >>>> ( fIXME ))||||(((term_to_parser "?LBRACKETGREATER?")***>parse_OPT_BAR***>parse_ROW_FIELD_LIST***>(term_to_parser "?RBRACKET?")) >>>> ( fIXME ))||||(((term_to_parser "?LBRACKETGREATER?")***>(term_to_parser "?RBRACKET?")) >>>> ( fIXME ))||||(((term_to_parser "?LBRACKETLESS?")***>parse_OPT_BAR***>parse_ROW_FIELD_LIST***>(term_to_parser "?RBRACKET?")) >>>> ( fIXME ))||||(((term_to_parser "?LBRACKETLESS?")***>parse_OPT_BAR***>parse_ROW_FIELD_LIST***>(term_to_parser "?GREATER?")***>parse_NAME_TAG_LIST***>(term_to_parser "?RBRACKET?")) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>(term_to_parser "?MODULE?")***>parse_PACKAGE_TYPE***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||((parse_EXTENSION) >>>> ( fIXME ))) i)) i)

 and parse_PACKAGE_TYPE = fun i -> (mcu4 tbl_PACKAGE_TYPE "PACKAGE_TYPE" (fun i -> (*unique4*) ((((parse_MTY_LONGIDENT) >>>> ( fIXME ))||||((parse_MTY_LONGIDENT***>(term_to_parser "?WITH?")***>parse_PACKAGE_TYPE_CSTRS) >>>> ( fIXME ))) i)) i)

 and parse_PACKAGE_TYPE_CSTR = fun i -> (mcu4 tbl_PACKAGE_TYPE_CSTR "PACKAGE_TYPE_CSTR" (fun i -> (*unique4*) (((((term_to_parser "?TYPE?")***>parse_LABEL_LONGIDENT***>(term_to_parser "?EQUAL?")***>parse_CORE_TYPE) >>>> ( fIXME ))) i)) i)

 and parse_PACKAGE_TYPE_CSTRS = fun i -> (mcu4 tbl_PACKAGE_TYPE_CSTRS "PACKAGE_TYPE_CSTRS" (fun i -> (*unique4*) ((((parse_PACKAGE_TYPE_CSTR) >>>> ( fIXME ))||||((parse_PACKAGE_TYPE_CSTR***>(term_to_parser "?AND?")***>parse_PACKAGE_TYPE_CSTRS) >>>> ( fIXME ))) i)) i)

 and parse_ROW_FIELD_LIST = fun i -> (mcu4 tbl_ROW_FIELD_LIST "ROW_FIELD_LIST" (fun i -> (*unique4*) ((((parse_ROW_FIELD) >>>> ( fIXME ))||||((parse_ROW_FIELD_LIST***>(term_to_parser "?BAR?")***>parse_ROW_FIELD) >>>> ( fIXME ))) i)) i)

 and parse_ROW_FIELD = fun i -> (mcu4 tbl_ROW_FIELD "ROW_FIELD" (fun i -> (*unique4*) ((((parse_TAG_FIELD) >>>> ( fIXME ))||||((parse_SIMPLE_CORE_TYPE) >>>> ( fIXME ))) i)) i)

 and parse_TAG_FIELD = fun i -> (mcu4 tbl_TAG_FIELD "TAG_FIELD" (fun i -> (*unique4*) ((((parse_NAME_TAG***>(term_to_parser "?OF?")***>parse_OPT_AMPERSAND***>parse_AMPER_TYPE_LIST) >>>> ( fIXME ))||||((parse_NAME_TAG) >>>> ( fIXME ))) i)) i)

 and parse_OPT_AMPERSAND = fun i -> (mcu4 tbl_OPT_AMPERSAND "OPT_AMPERSAND" (fun i -> (*unique4*) (((((term_to_parser "?AMPERSAND?")) >>>> ( fIXME ))||||(((term_to_parser "?eps?")) >>>> ( fIXME ))) i)) i)

 and parse_AMPER_TYPE_LIST = fun i -> (mcu4 tbl_AMPER_TYPE_LIST "AMPER_TYPE_LIST" (fun i -> (*unique4*) ((((parse_CORE_TYPE) >>>> ( fIXME ))||||((parse_AMPER_TYPE_LIST***>(term_to_parser "?AMPERSAND?")***>parse_CORE_TYPE) >>>> ( fIXME ))) i)) i)

 and parse_NAME_TAG_LIST = fun i -> (mcu4 tbl_NAME_TAG_LIST "NAME_TAG_LIST" (fun i -> (*unique4*) ((((parse_NAME_TAG) >>>> ( fIXME ))||||((parse_NAME_TAG_LIST***>parse_NAME_TAG) >>>> ( fIXME ))) i)) i)

 and parse_SIMPLE_CORE_TYPE_OR_TUPLE = fun i -> (mcu4 tbl_SIMPLE_CORE_TYPE_OR_TUPLE "SIMPLE_CORE_TYPE_OR_TUPLE" (fun i -> (*unique4*) ((((parse_SIMPLE_CORE_TYPE) >>>> ( fIXME ))||||((parse_SIMPLE_CORE_TYPE***>(term_to_parser "?STAR?")***>parse_CORE_TYPE_LIST) >>>> ( fIXME ))) i)) i)

 and parse_SIMPLE_CORE_TYPE_OR_TUPLE_NO_ATTR = fun i -> (mcu4 tbl_SIMPLE_CORE_TYPE_OR_TUPLE_NO_ATTR "SIMPLE_CORE_TYPE_OR_TUPLE_NO_ATTR" (fun i -> (*unique4*) ((((parse_SIMPLE_CORE_TYPE_NO_ATTR) >>>> ( fIXME ))||||((parse_SIMPLE_CORE_TYPE_NO_ATTR***>(term_to_parser "?STAR?")***>parse_CORE_TYPE_LIST_NO_ATTR) >>>> ( fIXME ))) i)) i)

 and parse_CORE_TYPE_COMMA_LIST = fun i -> (mcu4 tbl_CORE_TYPE_COMMA_LIST "CORE_TYPE_COMMA_LIST" (fun i -> (*unique4*) ((((parse_CORE_TYPE) >>>> ( fIXME ))||||((parse_CORE_TYPE_COMMA_LIST***>(term_to_parser "?COMMA?")***>parse_CORE_TYPE) >>>> ( fIXME ))) i)) i)

 and parse_SIMPLE_CORE_TYPE = fun i -> (mcu4 tbl_SIMPLE_CORE_TYPE "SIMPLE_CORE_TYPE" (fun i -> (*unique4*) ((((parse_SIMPLE_CORE_TYPE2) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>parse_CORE_TYPE_COMMA_LIST***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||((parse_SIMPLE_CORE_TYPE***>parse_ATTRIBUTE) >>>> ( fIXME ))) i)) i)

 and parse_CORE_TYPE_LIST_NO_ATTR = fun i -> (mcu4 tbl_CORE_TYPE_LIST_NO_ATTR "CORE_TYPE_LIST_NO_ATTR" (fun i -> (*unique4*) ((((parse_SIMPLE_CORE_TYPE_NO_ATTR) >>>> ( fIXME ))||||((parse_CORE_TYPE_LIST***>(term_to_parser "?STAR?")***>parse_SIMPLE_CORE_TYPE_NO_ATTR) >>>> ( fIXME ))) i)) i)

 and parse_CORE_TYPE_LIST = fun i -> (mcu4 tbl_CORE_TYPE_LIST "CORE_TYPE_LIST" (fun i -> (*unique4*) ((((parse_SIMPLE_CORE_TYPE) >>>> ( fIXME ))||||((parse_CORE_TYPE_LIST***>(term_to_parser "?STAR?")***>parse_SIMPLE_CORE_TYPE) >>>> ( fIXME ))) i)) i)

 and parse_SIMPLE_CORE_TYPE_NO_ATTR = fun i -> (mcu4 tbl_SIMPLE_CORE_TYPE_NO_ATTR "SIMPLE_CORE_TYPE_NO_ATTR" (fun i -> (*unique4*) ((((parse_SIMPLE_CORE_TYPE2) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>parse_CORE_TYPE_COMMA_LIST***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))) i)) i)

 and parse_METH_LIST = fun i -> (mcu4 tbl_METH_LIST "METH_LIST" (fun i -> (*unique4*) ((((parse_FIELD***>(term_to_parser "?SEMI?")***>parse_METH_LIST) >>>> ( fIXME ))||||((parse_FIELD***>parse_OPT_SEMI) >>>> ( fIXME ))||||(((term_to_parser "?DOTDOT?")) >>>> ( fIXME ))) i)) i)

 and parse_FIELD = fun i -> (mcu4 tbl_FIELD "FIELD" (fun i -> (*unique4*) ((((parse_LABEL***>(term_to_parser "?COLON?")***>parse_POLY_TYPE) >>>> ( fIXME ))) i)) i)

 and parse_POLY_TYPE = fun i -> (mcu4 tbl_POLY_TYPE "POLY_TYPE" (fun i -> (*unique4*) ((((parse_CORE_TYPE) >>>> ( fIXME ))||||((parse_TYPEVAR_LIST***>(term_to_parser "?DOT?")***>parse_CORE_TYPE) >>>> ( fIXME ))) i)) i)

 and parse_LABEL = fun i -> (mcu4 tbl_LABEL "LABEL" (fun i -> (*unique4*) (((((term_to_parser "?LIDENT?")) >>>> ( fIXME ))) i)) i)

 and parse_CONSTANT = fun i -> (mcu4 tbl_CONSTANT "CONSTANT" (fun i -> (*unique4*) (((((term_to_parser "?INT?")) >>>> ( fIXME ))||||(((term_to_parser "?CHAR?")) >>>> ( fIXME ))||||(((term_to_parser "?STRING?")) >>>> ( fIXME ))||||(((term_to_parser "?FLOAT?")) >>>> ( fIXME ))||||(((term_to_parser "?INT32?")) >>>> ( fIXME ))||||(((term_to_parser "?INT64?")) >>>> ( fIXME ))||||(((term_to_parser "?NATIVEINT?")) >>>> ( fIXME ))) i)) i)

 and parse_SIGNED_CONSTANT = fun i -> (mcu4 tbl_SIGNED_CONSTANT "SIGNED_CONSTANT" (fun i -> (*unique4*) ((((parse_CONSTANT) >>>> ( fIXME ))||||(((term_to_parser "?MINUS?")***>(term_to_parser "?INT?")) >>>> ( fIXME ))||||(((term_to_parser "?MINUS?")***>(term_to_parser "?FLOAT?")) >>>> ( fIXME ))||||(((term_to_parser "?MINUS?")***>(term_to_parser "?INT32?")) >>>> ( fIXME ))||||(((term_to_parser "?MINUS?")***>(term_to_parser "?INT64?")) >>>> ( fIXME ))||||(((term_to_parser "?MINUS?")***>(term_to_parser "?NATIVEINT?")) >>>> ( fIXME ))||||(((term_to_parser "?PLUS?")***>(term_to_parser "?INT?")) >>>> ( fIXME ))||||(((term_to_parser "?PLUS?")***>(term_to_parser "?FLOAT?")) >>>> ( fIXME ))||||(((term_to_parser "?PLUS?")***>(term_to_parser "?INT32?")) >>>> ( fIXME ))||||(((term_to_parser "?PLUS?")***>(term_to_parser "?INT64?")) >>>> ( fIXME ))||||(((term_to_parser "?PLUS?")***>(term_to_parser "?NATIVEINT?")) >>>> ( fIXME ))) i)) i)

 and parse_OPERATOR = fun i -> (mcu4 tbl_OPERATOR "OPERATOR" (fun i -> (*unique4*) (((((term_to_parser "?PREFIXOP?")) >>>> ( fIXME ))||||(((term_to_parser "?INFIXOP0?")) >>>> ( fIXME ))||||(((term_to_parser "?INFIXOP1?")) >>>> ( fIXME ))||||(((term_to_parser "?INFIXOP2?")) >>>> ( fIXME ))||||(((term_to_parser "?INFIXOP3?")) >>>> ( fIXME ))||||(((term_to_parser "?INFIXOP4?")) >>>> ( fIXME ))||||(((term_to_parser "?BANG?")) >>>> ( fIXME ))||||(((term_to_parser "?PLUS?")) >>>> ( fIXME ))||||(((term_to_parser "?PLUSDOT?")) >>>> ( fIXME ))||||(((term_to_parser "?MINUS?")) >>>> ( fIXME ))||||(((term_to_parser "?MINUSDOT?")) >>>> ( fIXME ))||||(((term_to_parser "?STAR?")) >>>> ( fIXME ))||||(((term_to_parser "?EQUAL?")) >>>> ( fIXME ))||||(((term_to_parser "?LESS?")) >>>> ( fIXME ))||||(((term_to_parser "?GREATER?")) >>>> ( fIXME ))||||(((term_to_parser "?OR?")) >>>> ( fIXME ))||||(((term_to_parser "?BARBAR?")) >>>> ( fIXME ))||||(((term_to_parser "?AMPERSAND?")) >>>> ( fIXME ))||||(((term_to_parser "?AMPERAMPER?")) >>>> ( fIXME ))||||(((term_to_parser "?COLONEQUAL?")) >>>> ( fIXME ))||||(((term_to_parser "?PERCENT?")) >>>> ( fIXME ))) i)) i)

 and parse_CONSTR_IDENT = fun i -> (mcu4 tbl_CONSTR_IDENT "CONSTR_IDENT" (fun i -> (*unique4*) (((((term_to_parser "?UIDENT?")) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||(((term_to_parser "?COLONCOLON?")) >>>> ( fIXME ))||||(((term_to_parser "?FALSE?")) >>>> ( fIXME ))||||(((term_to_parser "?TRUE?")) >>>> ( fIXME ))) i)) i)

 and parse_VAL_IDENT = fun i -> (mcu4 tbl_VAL_IDENT "VAL_IDENT" (fun i -> (*unique4*) (((((term_to_parser "?LIDENT?")) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>parse_OPERATOR***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))) i)) i)

 and parse_CONSTR_LONGIDENT = fun i -> (mcu4 tbl_CONSTR_LONGIDENT "CONSTR_LONGIDENT" (fun i -> (*unique4*) ((((parse_MOD_LONGIDENT) >>>> ( fIXME ))||||(((term_to_parser "?LBRACKET?")***>(term_to_parser "?RBRACKET?")) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||(((term_to_parser "?FALSE?")) >>>> ( fIXME ))||||(((term_to_parser "?TRUE?")) >>>> ( fIXME ))) i)) i)

 and parse_LABEL_LONGIDENT = fun i -> (mcu4 tbl_LABEL_LONGIDENT "LABEL_LONGIDENT" (fun i -> (*unique4*) (((((term_to_parser "?LIDENT?")) >>>> ( fIXME ))||||((parse_MOD_LONGIDENT***>(term_to_parser "?DOT?")***>(term_to_parser "?LIDENT?")) >>>> ( fIXME ))) i)) i)

 and parse_TYPE_LONGIDENT = fun i -> (mcu4 tbl_TYPE_LONGIDENT "TYPE_LONGIDENT" (fun i -> (*unique4*) (((((term_to_parser "?LIDENT?")) >>>> ( fIXME ))||||((parse_MOD_EXT_LONGIDENT***>(term_to_parser "?DOT?")***>(term_to_parser "?LIDENT?")) >>>> ( fIXME ))) i)) i)

 and parse_MTY_LONGIDENT = fun i -> (mcu4 tbl_MTY_LONGIDENT "MTY_LONGIDENT" (fun i -> (*unique4*) ((((parse_IDENT) >>>> ( fIXME ))||||((parse_MOD_EXT_LONGIDENT***>(term_to_parser "?DOT?")***>parse_IDENT) >>>> ( fIXME ))) i)) i)

 and parse_CLTY_LONGIDENT = fun i -> (mcu4 tbl_CLTY_LONGIDENT "CLTY_LONGIDENT" (fun i -> (*unique4*) (((((term_to_parser "?LIDENT?")) >>>> ( fIXME ))||||((parse_MOD_EXT_LONGIDENT***>(term_to_parser "?DOT?")***>(term_to_parser "?LIDENT?")) >>>> ( fIXME ))) i)) i)

 and parse_MOD_EXT_LONGIDENT = fun i -> (mcu4 tbl_MOD_EXT_LONGIDENT "MOD_EXT_LONGIDENT" (fun i -> (*unique4*) (((((term_to_parser "?UIDENT?")) >>>> ( fIXME ))||||((parse_MOD_EXT_LONGIDENT***>(term_to_parser "?DOT?")***>(term_to_parser "?UIDENT?")) >>>> ( fIXME ))||||((parse_MOD_EXT_LONGIDENT***>(term_to_parser "?LPAREN?")***>parse_MOD_EXT_LONGIDENT***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))) i)) i)

 and parse_CLASS_LONGIDENT = fun i -> (mcu4 tbl_CLASS_LONGIDENT "CLASS_LONGIDENT" (fun i -> (*unique4*) (((((term_to_parser "?LIDENT?")) >>>> ( fIXME ))||||((parse_MOD_LONGIDENT***>(term_to_parser "?DOT?")***>(term_to_parser "?LIDENT?")) >>>> ( fIXME ))) i)) i)

 and parse_MOD_LONGIDENT = fun i -> (mcu4 tbl_MOD_LONGIDENT "MOD_LONGIDENT" (fun i -> (*unique4*) (((((term_to_parser "?UIDENT?")) >>>> ( fIXME ))||||((parse_MOD_LONGIDENT***>(term_to_parser "?DOT?")***>(term_to_parser "?UIDENT?")) >>>> ( fIXME ))) i)) i)

 and parse_VAL_LONGIDENT = fun i -> (mcu4 tbl_VAL_LONGIDENT "VAL_LONGIDENT" (fun i -> (*unique4*) ((((parse_VAL_IDENT) >>>> ( fIXME ))||||((parse_MOD_LONGIDENT***>(term_to_parser "?DOT?")***>parse_VAL_IDENT) >>>> ( fIXME ))) i)) i)

 and parse_TOPLEVEL_DIRECTIVE = fun i -> (mcu4 tbl_TOPLEVEL_DIRECTIVE "TOPLEVEL_DIRECTIVE" (fun i -> (*unique4*) (((((term_to_parser "?SHARP?")***>parse_IDENT) >>>> ( fIXME ))||||(((term_to_parser "?SHARP?")***>parse_IDENT***>(term_to_parser "?STRING?")) >>>> ( fIXME ))||||(((term_to_parser "?SHARP?")***>parse_IDENT***>(term_to_parser "?INT?")) >>>> ( fIXME ))||||(((term_to_parser "?SHARP?")***>parse_IDENT***>parse_VAL_LONGIDENT) >>>> ( fIXME ))||||(((term_to_parser "?SHARP?")***>parse_IDENT***>(term_to_parser "?FALSE?")) >>>> ( fIXME ))||||(((term_to_parser "?SHARP?")***>parse_IDENT***>(term_to_parser "?TRUE?")) >>>> ( fIXME ))) i)) i)

 and parse_NAME_TAG = fun i -> (mcu4 tbl_NAME_TAG "NAME_TAG" (fun i -> (*unique4*) (((((term_to_parser "?BACKQUOTE?")***>parse_IDENT) >>>> ( fIXME ))) i)) i)

 and parse_IDENT = fun i -> (mcu4 tbl_IDENT "IDENT" (fun i -> (*unique4*) (((((term_to_parser "?UIDENT?")) >>>> ( fIXME ))||||(((term_to_parser "?LIDENT?")) >>>> ( fIXME ))) i)) i)

 and parse_REC_FLAG = fun i -> (mcu4 tbl_REC_FLAG "REC_FLAG" (fun i -> (*unique4*) (((((term_to_parser "?eps?")) >>>> ( fIXME ))||||(((term_to_parser "?REC?")) >>>> ( fIXME ))) i)) i)

 and parse_DIRECTION_FLAG = fun i -> (mcu4 tbl_DIRECTION_FLAG "DIRECTION_FLAG" (fun i -> (*unique4*) (((((term_to_parser "?TO?")) >>>> ( fIXME ))||||(((term_to_parser "?DOWNTO?")) >>>> ( fIXME ))) i)) i)

 and parse_PRIVATE_FLAG = fun i -> (mcu4 tbl_PRIVATE_FLAG "PRIVATE_FLAG" (fun i -> (*unique4*) (((((term_to_parser "?eps?")) >>>> ( fIXME ))||||(((term_to_parser "?PRIVATE?")) >>>> ( fIXME ))) i)) i)

 and parse_MUTABLE_FLAG = fun i -> (mcu4 tbl_MUTABLE_FLAG "MUTABLE_FLAG" (fun i -> (*unique4*) (((((term_to_parser "?eps?")) >>>> ( fIXME ))||||(((term_to_parser "?MUTABLE?")) >>>> ( fIXME ))) i)) i)

 and parse_VIRTUAL_FLAG = fun i -> (mcu4 tbl_VIRTUAL_FLAG "VIRTUAL_FLAG" (fun i -> (*unique4*) (((((term_to_parser "?eps?")) >>>> ( fIXME ))||||(((term_to_parser "?VIRTUAL?")) >>>> ( fIXME ))) i)) i)

 and parse_PRIVATE_VIRTUAL_FLAGS = fun i -> (mcu4 tbl_PRIVATE_VIRTUAL_FLAGS "PRIVATE_VIRTUAL_FLAGS" (fun i -> (*unique4*) (((((term_to_parser "?eps?")) >>>> ( fIXME ))||||(((term_to_parser "?PRIVATE?")) >>>> ( fIXME ))||||(((term_to_parser "?VIRTUAL?")) >>>> ( fIXME ))||||(((term_to_parser "?PRIVATE?")***>(term_to_parser "?VIRTUAL?")) >>>> ( fIXME ))||||(((term_to_parser "?VIRTUAL?")***>(term_to_parser "?PRIVATE?")) >>>> ( fIXME ))) i)) i)

 and parse_OVERRIDE_FLAG = fun i -> (mcu4 tbl_OVERRIDE_FLAG "OVERRIDE_FLAG" (fun i -> (*unique4*) (((((term_to_parser "?eps?")) >>>> ( fIXME ))||||(((term_to_parser "?BANG?")) >>>> ( fIXME ))) i)) i)

 and parse_OPT_BAR = fun i -> (mcu4 tbl_OPT_BAR "OPT_BAR" (fun i -> (*unique4*) (((((term_to_parser "?eps?")) >>>> ( fIXME ))||||(((term_to_parser "?BAR?")) >>>> ( fIXME ))) i)) i)

 and parse_OPT_SEMI = fun i -> (mcu4 tbl_OPT_SEMI "OPT_SEMI" (fun i -> (*unique4*) (((((term_to_parser "?eps?")) >>>> ( fIXME ))||||(((term_to_parser "?SEMI?")) >>>> ( fIXME ))) i)) i)

 and parse_SUBTRACTIVE = fun i -> (mcu4 tbl_SUBTRACTIVE "SUBTRACTIVE" (fun i -> (*unique4*) (((((term_to_parser "?MINUS?")) >>>> ( fIXME ))||||(((term_to_parser "?MINUSDOT?")) >>>> ( fIXME ))) i)) i)

 and parse_ADDITIVE = fun i -> (mcu4 tbl_ADDITIVE "ADDITIVE" (fun i -> (*unique4*) (((((term_to_parser "?PLUS?")) >>>> ( fIXME ))||||(((term_to_parser "?PLUSDOT?")) >>>> ( fIXME ))) i)) i)

 and parse_SINGLE_ATTR_ID = fun i -> (mcu4 tbl_SINGLE_ATTR_ID "SINGLE_ATTR_ID" (fun i -> (*unique4*) (((((term_to_parser "?LIDENT?")) >>>> ( fIXME ))||||(((term_to_parser "?UIDENT?")) >>>> ( fIXME ))||||(((term_to_parser "?AND?")) >>>> ( fIXME ))||||(((term_to_parser "?AS?")) >>>> ( fIXME ))||||(((term_to_parser "?ASSERT?")) >>>> ( fIXME ))||||(((term_to_parser "?BEGIN?")) >>>> ( fIXME ))||||(((term_to_parser "?CLASS?")) >>>> ( fIXME ))||||(((term_to_parser "?CONSTRAINT?")) >>>> ( fIXME ))||||(((term_to_parser "?DO?")) >>>> ( fIXME ))||||(((term_to_parser "?DONE?")) >>>> ( fIXME ))||||(((term_to_parser "?DOWNTO?")) >>>> ( fIXME ))||||(((term_to_parser "?ELSE?")) >>>> ( fIXME ))||||(((term_to_parser "?END?")) >>>> ( fIXME ))||||(((term_to_parser "?EXCEPTION?")) >>>> ( fIXME ))||||(((term_to_parser "?EXTERNAL?")) >>>> ( fIXME ))||||(((term_to_parser "?FALSE?")) >>>> ( fIXME ))||||(((term_to_parser "?FOR?")) >>>> ( fIXME ))||||(((term_to_parser "?FUN?")) >>>> ( fIXME ))||||(((term_to_parser "?FUNCTION?")) >>>> ( fIXME ))||||(((term_to_parser "?FUNCTOR?")) >>>> ( fIXME ))||||(((term_to_parser "?IF?")) >>>> ( fIXME ))||||(((term_to_parser "?IN?")) >>>> ( fIXME ))||||(((term_to_parser "?INCLUDE?")) >>>> ( fIXME ))||||(((term_to_parser "?INHERIT?")) >>>> ( fIXME ))||||(((term_to_parser "?INITIALIZER?")) >>>> ( fIXME ))||||(((term_to_parser "?LAZY?")) >>>> ( fIXME ))||||(((term_to_parser "?LET?")) >>>> ( fIXME ))||||(((term_to_parser "?MATCH?")) >>>> ( fIXME ))||||(((term_to_parser "?METHOD?")) >>>> ( fIXME ))||||(((term_to_parser "?MODULE?")) >>>> ( fIXME ))||||(((term_to_parser "?MUTABLE?")) >>>> ( fIXME ))||||(((term_to_parser "?NEW?")) >>>> ( fIXME ))||||(((term_to_parser "?OBJECT?")) >>>> ( fIXME ))||||(((term_to_parser "?OF?")) >>>> ( fIXME ))||||(((term_to_parser "?OPEN?")) >>>> ( fIXME ))||||(((term_to_parser "?OR?")) >>>> ( fIXME ))||||(((term_to_parser "?PRIVATE?")) >>>> ( fIXME ))||||(((term_to_parser "?REC?")) >>>> ( fIXME ))||||(((term_to_parser "?SIG?")) >>>> ( fIXME ))||||(((term_to_parser "?STRUCT?")) >>>> ( fIXME ))||||(((term_to_parser "?THEN?")) >>>> ( fIXME ))||||(((term_to_parser "?TO?")) >>>> ( fIXME ))||||(((term_to_parser "?TRUE?")) >>>> ( fIXME ))||||(((term_to_parser "?TRY?")) >>>> ( fIXME ))||||(((term_to_parser "?TYPE?")) >>>> ( fIXME ))||||(((term_to_parser "?VAL?")) >>>> ( fIXME ))||||(((term_to_parser "?VIRTUAL?")) >>>> ( fIXME ))||||(((term_to_parser "?WHEN?")) >>>> ( fIXME ))||||(((term_to_parser "?WHILE?")) >>>> ( fIXME ))||||(((term_to_parser "?WITH?")) >>>> ( fIXME ))) i)) i)

 and parse_POST_ITEM_ATTRIBUTE = fun i -> (mcu4 tbl_POST_ITEM_ATTRIBUTE "POST_ITEM_ATTRIBUTE" (fun i -> (*unique4*) (((((term_to_parser "?LBRACKETATAT?")***>parse_ATTR_ID***>parse_PAYLOAD***>(term_to_parser "?RBRACKET?")) >>>> ( fIXME ))) i)) i)

 and parse_POST_ITEM_ATTRIBUTES = fun i -> (mcu4 tbl_POST_ITEM_ATTRIBUTES "POST_ITEM_ATTRIBUTES" (fun i -> (*unique4*) (((((term_to_parser "?eps?")) >>>> ( fIXME ))||||((parse_POST_ITEM_ATTRIBUTE***>parse_POST_ITEM_ATTRIBUTES) >>>> ( fIXME ))) i)) i)

 and parse_ATTRIBUTE = fun i -> (mcu4 tbl_ATTRIBUTE "ATTRIBUTE" (fun i -> (*unique4*) (((((term_to_parser "?LBRACKETAT?")***>parse_ATTR_ID***>parse_PAYLOAD***>(term_to_parser "?RBRACKET?")) >>>> ( fIXME ))) i)) i)

 and parse_EXT_ATTRIBUTES = fun i -> (mcu4 tbl_EXT_ATTRIBUTES "EXT_ATTRIBUTES" (fun i -> (*unique4*) (((((term_to_parser "?eps?")) >>>> ( fIXME ))||||((parse_ATTRIBUTE***>parse_ATTRIBUTES) >>>> ( fIXME ))||||(((term_to_parser "?PERCENT?")***>parse_ATTR_ID***>parse_ATTRIBUTES) >>>> ( fIXME ))) i)) i)

 and parse_ATTRIBUTES = fun i -> (mcu4 tbl_ATTRIBUTES "ATTRIBUTES" (fun i -> (*unique4*) (((((term_to_parser "?eps?")) >>>> ( fIXME ))||||((parse_ATTRIBUTE***>parse_ATTRIBUTES) >>>> ( fIXME ))) i)) i)

 and parse_EXTENSION = fun i -> (mcu4 tbl_EXTENSION "EXTENSION" (fun i -> (*unique4*) (((((term_to_parser "?LBRACKETPERCENT?")***>parse_ATTR_ID***>parse_PAYLOAD***>(term_to_parser "?RBRACKET?")) >>>> ( fIXME ))) i)) i)

 and parse_ITEM_EXTENSION = fun i -> (mcu4 tbl_ITEM_EXTENSION "ITEM_EXTENSION" (fun i -> (*unique4*) (((((term_to_parser "?LBRACKETPERCENTPERCENT?")***>parse_ATTR_ID***>parse_PAYLOAD***>(term_to_parser "?RBRACKET?")) >>>> ( fIXME ))) i)) i)

 and parse_ATTR_ID = fun i -> (mcu4 tbl_ATTR_ID "ATTR_ID" (fun i -> (*unique4*) ((((parse_SINGLE_ATTR_ID) >>>> ( fIXME ))||||((parse_SINGLE_ATTR_ID***>(term_to_parser "?DOT?")***>parse_ATTR_ID) >>>> ( fIXME ))) i)) i)

 and parse_STRUCTURE = fun i -> (mcu4 tbl_STRUCTURE "STRUCTURE" (fun i -> (*unique4*) ((((parse_STR_ATTRIBUTE***>parse_STRUCTURE) >>>> ( fIXME ))||||((parse_SEQ_EXPR***>parse_POST_ITEM_ATTRIBUTES***>parse_STRUCTURE_TAIL) >>>> ( fIXME ))||||((parse_STRUCTURE_TAIL) >>>> ( fIXME ))) i)) i)

 and parse_CORE_TYPE = fun i -> (mcu4 tbl_CORE_TYPE "CORE_TYPE" (fun i -> (*unique4*) ((((parse_CORE_TYPE2) >>>> ( fIXME ))||||((parse_CORE_TYPE2***>(term_to_parser "?AS?")***>(term_to_parser "?QUOTE?")***>parse_IDENT) >>>> ( fIXME ))) i)) i)

 and parse_PAYLOAD = fun i -> (mcu4 tbl_PAYLOAD "PAYLOAD" (fun i -> (*unique4*) ((((parse_STRUCTURE) >>>> ( fIXME ))||||(((term_to_parser "?COLON?")***>parse_CORE_TYPE) >>>> ( fIXME ))||||(((term_to_parser "?QUESTION?")***>parse_PATTERN) >>>> ( fIXME ))||||(((term_to_parser "?QUESTION?")***>parse_PATTERN***>(term_to_parser "?WHEN?")***>parse_SEQ_EXPR) >>>> ( fIXME ))) i)) i)

 and parse_PATTERN = fun i -> (mcu4 tbl_PATTERN "PATTERN" (fun i -> (*unique4*) ((((parse_SIMPLE_PATTERN) >>>> ( fIXME ))||||((parse_PATTERN***>(term_to_parser "?AS?")***>parse_VAL_IDENT) >>>> ( fIXME ))||||((parse_PATTERN_COMMA_LIST) >>>> ( fIXME ))||||((parse_CONSTR_LONGIDENT***>parse_PATTERN) >>>> ( fIXME ))||||((parse_NAME_TAG***>parse_PATTERN) >>>> ( fIXME ))||||((parse_PATTERN***>(term_to_parser "?COLONCOLON?")***>parse_PATTERN) >>>> ( fIXME ))||||(((term_to_parser "?LPAREN?")***>(term_to_parser "?COLONCOLON?")***>(term_to_parser "?RPAREN?")***>(term_to_parser "?LPAREN?")***>parse_PATTERN***>(term_to_parser "?COMMA?")***>parse_PATTERN***>(term_to_parser "?RPAREN?")) >>>> ( fIXME ))||||((parse_PATTERN***>(term_to_parser "?BAR?")***>parse_PATTERN) >>>> ( fIXME ))||||(((term_to_parser "?LAZY?")***>parse_SIMPLE_PATTERN) >>>> ( fIXME ))||||((parse_PATTERN***>parse_ATTRIBUTE) >>>> ( fIXME ))) i)) i)

 and parse_SEQ_EXPR = fun i -> (mcu4 tbl_SEQ_EXPR "SEQ_EXPR" (fun i -> (*unique4*) ((((parse_EXPR) >>>> ( fIXME ))||||((parse_EXPR***>(term_to_parser "?SEMI?")) >>>> ( fIXME ))||||((parse_EXPR***>(term_to_parser "?SEMI?")***>parse_SEQ_EXPR) >>>> ( fIXME ))) i)) i)
let parse_start = parse_IMPLEMENTATION
(* p3post.ml *)

let toks = 
  let open Parser in
  let open Lexer in
  let s = read_file_as_string args.input in
  let eof = EOF in
  let list_of_lexing s0 = (
    let rec f1 xs s0 = (
      let tok = Lexer.token s0 in
      if tok = eof then tok::xs else f1 (tok::xs) s0)
    in
    List.rev (f1 [] s0))
  in
  let s0 = (Lexing.from_string s) in    
  let toks = Array.of_list (list_of_lexing s0) in
  toks

let toks = Array.map tokint_of_token toks


let parse_expression () = (
  (* the following for parsing an expression *)
  let p = parse_PARSE_EXPRESSION in
  let (o,tmo) = oracle_of_parser p toks (Array.length toks) in
  (* check that the oracle is vaguely sensible *)
  let (sym1,sym2) = (`NT (mk_bstring true "SEQ_EXPR"),`TM (mk_bstring false "?EOF?")) in
  let n = o (sym1,sym2) (0,Array.length toks) in 
  let _ = assert (n=[(Array.length toks) - 1]) in
  let _ = print_endline "Parsed OK" in
  ())

let parse_implementation () = (
  (* the following for parsing an expression *)
  let p = parse_IMPLEMENTATION in
  let (o,tmo) = oracle_of_parser p toks (Array.length toks) in
  (* check that the oracle is vaguely sensible *)
  let (sym1,sym2) = (`NT (mk_bstring true "STRUCTURE"),`TM (mk_bstring false "?EOF?")) in
  let n = o (sym1,sym2) (0,Array.length toks) in 
  let _ = assert (n=[(Array.length toks) - 1]) in
  let _ = print_endline "Parsed OK" in
  ())

let main () = (parse_implementation ())

let _ = main ()

(*

#print_depth 10000;;
#print_length 10000;;


*)
